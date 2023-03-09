module Perun.Adjudicator.Adjudicator
  ( adjudicator,
    adjudicatorSubscription,
    AdjudicatorSchema,
  )
where

import Control.Lens hiding (index, para)
import Control.Monad (unless)
import Control.Monad.Error.Lens
import Data.Either (rights)
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text, pack)
import Ledger hiding (ChainIndexTxOut)
import qualified Ledger.Tx as L
import Perun.Adjudicator.History
import Perun.Error
import Perun.Offchain
import Perun.Offchain.ChannelTxPool
import Perun.Offchain.Event
import Perun.Offchain.State
import Perun.Onchain
import Plutus.ChainIndex hiding (tip, txFromTxId)
import Plutus.Contract
import Plutus.Contract.Types (Promise (..))
import Plutus.Contract.Util (loopM)
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Typed
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import qualified PlutusTx

type AdjudicatorSchema = Endpoint "watch" ()

-- | adjudicatorSubscription listens for state changes to the onchain state of
-- the channel. The change can be viewed by subscribers of the PAB instance.
adjudicatorSubscription ::
  ChannelID ->
  Contract PerunEvent AdjudicatorSchema PerunError ()
adjudicatorSubscription cID = do
  let stop = pure $ Right ()
      continue = pure . Left
      -- onEvents is the callback invoked when the state changes and new events
      -- were fired.
      onEvents evs@[Concluded _] = do
        tellAndLog cID evs
        -- We stop listening for state changes after the channel has been
        -- concluded.
        stop
      onEvents evs = do
        tellAndLog cID evs
        adjudicator cID >>= continue
      -- loop listens for state changes for the channel in question. It
      -- receives the previous state and listens for the next state change.
      loop = loopM onEvents
  initial <- getOnChainState cID
  case initial of
    Right cs -> do
      -- We found an initial channel state, so we fetch all past events before
      -- continuing listening for new state changes.
      logInfo @String $ unwords ["found initial state for channel:", show cID]
      -- TODO: We do not want to `head` here but instead we should handle
      -- all possible chain event lists.
      getAllOnChainEvents cID >>= loop . head
    Left NoOnchainStatesFoundErr -> do
      -- The channel in question has no onchain state, so we wait for the
      -- creation of the channel.
      logInfo @String $ unwords ["found no state for channel:", show cID]
      adjudicator cID >>= loop
    Left err -> logError @String $ unwords ["adjudicator error:", show err]

-- | tellAndLog is a helper to log the events and tell them to subscribers of
-- the adjudicator contract.
tellAndLog ::
  (AsPerunError e, AsContractError e) =>
  ChannelID ->
  [ChannelEvent] ->
  Contract PerunEvent AdjudicatorSchema e ()
tellAndLog cid events = do
  mapM_ log events
  tell events
  where
    log Created {} = logInfo @String $ unwords ["Created channel", show cid]
    log Deposited {} = logInfo @String $ unwords ["Deposited into channel", show cid]
    log Disputed {} = logInfo @String $ unwords ["Disputed channel", show cid]
    log Progressed {} = logInfo @String $ unwords ["Progressed channel", show cid]
    log Concluded {} = logInfo @String $ unwords ["Concluded channel", show cid]
    log Withdrawn = logInfo @String $ unwords ["Withdrawn from channel", show cid]

eventsFromChannelWaitingResult :: ChannelWaitingResult -> Either SubscriptionException [ChannelEvent]
eventsFromChannelWaitingResult (ChannelTransition action oldState newState) =
  deduceEvents $ ChannelStep action (perunState newState) ChannelNil
eventsFromChannelWaitingResult (ChannelCreated newState) = Right [Created . perunState $ newState]
eventsFromChannelWaitingResult (ChannelEnded finalState) = Right [Concluded . perunState $ finalState]

-- | adjudicator listens for state changes to the onchain state of the channel.
adjudicator ::
  (AsPerunError e, AsContractError e) =>
  ChannelID ->
  Contract PerunEvent AdjudicatorSchema e [ChannelEvent]
adjudicator cID = do
  updatePromise <- waitForUpdate cID
  let channelUpdate = promiseBind updatePromise $ \wr -> do
        case eventsFromChannelWaitingResult wr of
          Left err -> throwing _SubscriptionError err
          Right evs -> return evs
  awaitPromise channelUpdate

waitForUpdate ::
  (AsPerunError e, AsContractError e) =>
  ChannelID ->
  Contract
    PerunEvent
    s
    e
    (Promise PerunEvent s e ChannelWaitingResult)
waitForUpdate cID = do
  currentState <- getOnChainState cID
  let projectThree = (\(a, (b, c)) -> (a, b, c))
      projectFst = (\(a, (b, _)) -> (a, b))
      success = case currentState of
        Left NoOnchainStatesFoundErr -> do
          let addr = channelAddress cID
              -- waitForStart waits for the channel to be created.
              waitForStart = promiseBind (utxoIsProduced addr) $ \ciTx -> do
                logInfo ciTx
                logInfo @String $ unwords ["UTXO produced at:", show addr]
                outRefMaps <-
                  Control.Lens.traverse utxosTxOutTxFromTx ciTx >>= \case
                    -- ChainIndex was not synced properly to retrieve UTXOs for
                    -- ciTx, retry after sync.
                    r | null (concat r) -> retryAfterSync $ Control.Lens.traverse utxosTxOutTxFromTx ciTx
                    outRefMaps -> return outRefMaps
                let produced = getStart cID (map projectThree . concat $ outRefMaps)
                -- If UTXOs were produced at the channel address and DID NOT
                -- contain a start state, we continue waiting for channel
                -- creation.
                case produced of
                  Left NoStartStateFoundErr -> awaitPromise waitForStart
                  Right newOcs -> return $ ChannelCreated newOcs
                  Left _err -> throwing _SubscriptionError CorruptedChainIndexErr
          waitForStart
        Right currentOcs@(OnChainState ocsTxOutRef) -> do
          promiseBind (utxoIsSpent (Typed.tyTxOutRefRef ocsTxOutRef)) $ \txn -> do
            outRefMap <-
              map projectFst
                <$> ( utxosTxOutTxFromTx txn >>= \case
                        r | null r -> retryAfterSync $ utxosTxOutTxFromTx txn
                        result -> return result
                    )
            let newStates = getStates cID (typedChannelValidator cID) outRefMap
            case newStates of
              [] -> pure $ ChannelEnded currentOcs
              xs -> case chooser xs of
                Left err -> throwing _SubscriptionError err
                Right newState -> do
                  action <- parseChannelAction currentOcs txn
                  pure $ ChannelTransition action currentOcs newState
        Left err -> do
          -- All other errors are valid reasons to stop listening for updates.
          -- Errors can only be thrown inside the `Contract` monad, so we have
          -- to wrap the error in a `Promise` manually.
          Promise $ throwing _SubscriptionError err
  pure success

parseChannelAction ::
  (AsPerunError e, AsContractError e) =>
  OnChainState ->
  ChainIndexTx ->
  Contract PerunEvent s e ChannelAction
parseChannelAction (OnChainState inputRef) citx = do
  let inputs = citx ^. citxInputs
      ir = Typed.tyTxOutRefRef inputRef
  txIn <- case find ((== ir) . txInRef) inputs of
    Just txIn -> return txIn
    _else -> throwing _SubscriptionError CorruptedChainIndexErr
  rawAction <- case txInType txIn of
    Just (ConsumeScriptAddress (Versioned _s _) action _datum) ->
      return action
    _else -> throwing _SubscriptionError CorruptedChainIndexErr
  case PlutusTx.fromBuiltinData . getRedeemer $ rawAction of
    Just channelAction -> return channelAction
    Nothing -> throwing _SubscriptionError CorruptedChainIndexErr

-- | getAllOnChainEvents retrieves all on-chain events which might have occured
-- already for the given ChannelID.
--
-- NOTE:
-- We assume that ALL channels are unique and only used once. What is not
-- allowed to happen is:
--    <channel-id> was created
--    -> <channel-id> concluded
--    -> <channel-id> created again
getAllOnChainEvents ::
  (AsPerunError e, AsContractError e) =>
  ChannelID ->
  Contract PerunEvent s e [[ChannelEvent]]
getAllOnChainEvents cid = do
  syncToCurrentSlot
  txpoolsForChannel cid >>= mapM handlePastEvents
  where
    -- TODO: Somehow make use of the ChannelToken???
    handlePastEvents (ChannelTxPool _ct []) = do
      -- The channel was never created.
      return []
    handlePastEvents (ChannelTxPool _ct ctxs) = do
      events <- case generateHistory ctxs of
        Left err -> throwing _SubscriptionError err
        Right hist -> return $ deduceEvents hist
      case events of
        Left err -> throwing _SubscriptionError err
        Right evs -> return evs

-- | generateHistory creates a history for the given ChannelID using tx from
-- the given TxPool.
generateHistory :: [ChannelTx] -> Either SubscriptionException ChannelHistory
generateHistory [] = return ChannelNil
generateHistory hist = do
  -- channelGenesis should ALWAYS exist when we reach this path, otherwise
  -- the given history does not relate to a Perun channel, or something
  -- else went horribly wrong.
  genesis <- case filter isChannelGenesis hist of
    [] -> throwError CorruptedChainIndexErr
    [ctx] -> do
      -- We expect the resulting list to have exactly one element. If
      -- there are mutiple channel genesis transactions, we are in the
      -- realm of undefined behaviour.
      return $ mkChannelGenesis ctx
    _else -> throwError CorruptedChainIndexErr
  traceChannelHistory genesis . filter (isNotThisTx' $ generalizeFirst genesis) $ hist
  where
    isChannelGenesis :: ChannelTx -> Bool
    isChannelGenesis (ChannelTx _ Nothing (Just _)) = True
    isChannelGenesis ChannelTx {} = False
    traceChannelHistory ::
      ChannelTxFirst ->
      [ChannelTx] ->
      Either SubscriptionException ChannelHistory
    traceChannelHistory genesisTx@(ChannelTx _ _ (_, genesisDatum)) origTxPool =
      ChannelCreation genesisDatum <$> go (generalizeFirst genesisTx) origTxPool
      where
        go :: ChannelTx -> [ChannelTx] -> Either SubscriptionException ChannelHistory
        go _ [] = do
          -- No transactions in tx pool left, end of ChannelHistory.
          return ChannelNil
        go ctx txPool = do
          next <- case find (isNextTx ctx) txPool of
            Nothing -> do
              -- If we do not find a successor TX in the txPool AND
              -- have things left in the pool, something is not correct
              -- with the given history, we will abort here.
              throwError DivergentChannelHistory
            Just tx -> return tx
          case next of
            ChannelTx nCitx (Just (_iRef, action, _iChannelDatum)) (Just (_oRef, oChannelDatum)) -> do
              -- We have a channel update. Channel makes a step.
              ChannelStep action oChannelDatum <$> go next (filter (isNotThisTx nCitx) txPool)
            ChannelTx nCitx (Just (_iRef, action, iChannelDatum)) Nothing -> do
              -- We have a channel close. The transaction closing the channel
              -- has to reference the same state as the last state in the
              -- previous transaction.
              let isSameChannelDatum = do
                    cd <- ctx ^? channelTxOutRef . _Just . _2
                    return $ cd == iChannelDatum
              -- Throw an error if the channelstates do not match.
              unless (fromMaybe False isSameChannelDatum) $ throwError CorruptedChainIndexErr
              ChannelConclude action <$> go next (filter (isNotThisTx nCitx) txPool)
            -- nCitx has only a single ChannelDatum. Either the channel was
            -- concluded or we reached the local end of the ChannelHistory.
            ChannelTx _nCitx Nothing (Just _) -> do
              -- This is not possible, since the txPool should NOT contain the
              -- start of the channel when we reach this point.
              throwError CorruptedChainIndexErr
            _else -> do
              -- There is no next transaction but we still have txs left in the
              -- txPool.
              throwError DivergentChannelHistory -- TODO: Use correct error here.
    isNextTx :: ChannelTx -> ChannelTx -> Bool
    isNextTx (ChannelTx _citx _ o) (ChannelTx _nCitx ni _) =
      let isNextTx' = do
            ref <- fst <$> o
            nRef <- fst3 <$> ni
            return $ ref == nRef
       in fromMaybe False isNextTx'
    fst3 :: (a, b, c) -> a
    fst3 (a, _, _) = a
    isNotThisTx :: ChainIndexTx -> ChannelTx -> Bool
    isNotThisTx citx pCitx = citx /= (pCitx ^. channelcitx)
    isNotThisTx' :: ChannelTx -> ChannelTx -> Bool
    isNotThisTx' citx = isNotThisTx (citx ^. channelcitx)

deduceEvents :: ChannelHistory -> Either SubscriptionException [ChannelEvent]
deduceEvents = go Nothing
  where
    go _ ChannelNil = return []
    go _ (ChannelCreation d h) = (Created d :) <$> go (Just d) h
    go (Just d') (ChannelStep Fund d h) = (mkDepositList d d' ++) <$> go (Just d) h
    go _ (ChannelStep (MkDispute _) d h) = (Disputed d :) <$> go (Just d) h
    go (Just d') (ChannelConclude Abort h) = (Concluded d' :) <$> go (Just d') h
    go (Just d') (ChannelConclude ForceClose h) = (Concluded d' :) <$> go (Just d') h
    go (Just d') (ChannelConclude (MkClose _) h) = (Concluded d' :) <$> go (Just d') h
    go _ remainingHistory = throwError $ ImpossibleChannelHistory remainingHistory
    -- We currently expect every participant to deposit for himself. The
    -- resulting event list for deposits will always contain a single
    -- element as long as this restriction is in place.
    mkDepositList d d' =
      let newFunding = funding d
          prevFunding = funding d'
          lenPrevFunding = length prevFunding
       in [Deposited d' (fromIntegral lenPrevFunding) (drop lenPrevFunding newFunding)]

-- | getOnChainState returns the current onchain state for the given
-- ChannelID. It will throw a PerunError if the onchain state is malformed.
getOnChainState ::
  (AsPerunError e, AsContractError e) =>
  ChannelID ->
  Contract PerunEvent s e (Either SubscriptionException OnChainState)
getOnChainState cID = do
  utxoTx <- utxosAt $ channelAddress cID
  let states = getStates cID (typedChannelValidator cID) (Map.toList utxoTx)
  return $ chooser states

-- | chooser is responsible for resolving the current contractstate from a list
-- of UTXO's. Since there might be more than one UTXO available at a given
-- scriptaddress, a chooser has to be able to determine which of those states
-- is up to date and correct.
--
-- TODO: Implement a chooser which allows watching for the ThreadToken.
chooser :: [OnChainState] -> Either SubscriptionException OnChainState
chooser [ocs] = Right ocs
chooser [] = Left NoOnchainStatesFoundErr
chooser ocs =
  Left $
    InvalidOnchainStatesForChooserErr $
      pack . unwords $
        [ "Number of OnChainStates",
          show . length $ ocs
        ]

getStart :: ChannelID -> [(TxOutRef, L.ChainIndexTxOut, ChainIndexTx)] -> Either ChannelTxErr OnChainState
getStart cid refMap = do
  ctmap <- mapM resolveChannelToken refMap
  case find isStartState ctmap of
    Just (_, _, _, _, tyTxOutRef) -> return . OnChainState $ tyTxOutRef
    Nothing -> throwError NoStartStateFoundErr
  where
    tcv = typedChannelValidator cid
    resolveChannelToken (txOutRef, lCiTxOut, citx) = do
      let idx = fromIntegral $ txOutRefIdx txOutRef
      ciTxOut <- case citx ^? citxOutputs . _ValidTx . ix idx of
        Nothing -> throwError InvalidTxOutRefErr
        Just ciTxOut -> return ciTxOut
      let rawDatum = citoDatum ciTxOut
          val = citoValue ciTxOut
      d <- parseDatumFromOutputDatum citx rawDatum
      cd <- channelDatumFromDatum d
      tyTxOutRef <- case Typed.typeScriptTxOutRef tcv txOutRef (toTxOut lCiTxOut) d of
        Left _ -> throwError InvalidTxOutRefErr
        Right tyTxOutRef -> return tyTxOutRef
      return (ciTxOut, citx, d, cd, tyTxOutRef)
    isStartState (ciTxOut, _, d, cd, _) =
      let val = citoValue ciTxOut
       in hasValidThreadToken cid (channelToken cd) val && isValidDatum cid cd

getStates :: ChannelID -> Scripts.TypedValidator ChannelTypes -> [(TxOutRef, L.ChainIndexTxOut)] -> [OnChainState]
getStates cid tcv refMap =
  flip mapMaybe refMap $ \(txOutRef, lciTxOut) -> do
    let txOut = toTxOut lciTxOut
    datum <- lciTxOut ^? ciTxOutScriptDatum . _2 . _Just
    ocsTxOutRef <- either (const Nothing) Just $ do
      let val = lciTxOut ^. ciTxOutValue
      tyTxOutRef <- case Typed.typeScriptTxOutRef tcv txOutRef txOut datum of
        Left _ -> throwError InvalidTxOutRefErr
        Right tyTxOutRef -> return tyTxOutRef
      let ct = channelToken . Typed.tyTxOutData . Typed.tyTxOutRefOut $ tyTxOutRef
      unless (hasValidThreadToken cid ct val) $ throwError InvalidThreadTokenErr
      pure tyTxOutRef
    pure $ OnChainState ocsTxOutRef

-- | The result of waiting for a channel update.
data ChannelWaitingResult
  = ChannelEnded !OnChainState
  | ChannelCreated !OnChainState
  | ChannelTransition !ChannelAction !OnChainState !OnChainState
