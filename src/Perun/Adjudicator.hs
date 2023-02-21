module Perun.Adjudicator
  ( adjudicator,
    adjudicatorSubscription,
    AdjudicatorSchema,
  )
where

import Control.Lens hiding (index, para)
import Control.Monad (unless)
import Control.Monad.Error.Lens
import Data.Default
import Data.Either
import Data.Functor.Foldable (ListF (..), para)
import Data.List (find, genericSplitAt)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text, pack)
import Ledger hiding (ChainIndexTxOut)
import qualified Ledger.Tx as L
import Perun.Error
import Perun.Offchain
import Perun.Offchain.ChannelTxPool
import Perun.Offchain.Event
import Perun.Offchain.State
import Perun.Onchain
import Plutus.ChainIndex hiding (tip, txFromTxId)
import Plutus.ChainIndex.Api
import Plutus.Contract
import Plutus.Contract.Request
import Plutus.Contract.Util (loopM)
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Typed
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import Plutus.V1.Ledger.Api (BuiltinByteString)
import PlutusTx

type AdjudicatorSchema =
  Endpoint "watch" ()
    .\/ Endpoint "abort" AbortParams
    .\/ Endpoint "dispute" DisputeParams
    .\/ Endpoint "close" CloseParams
    .\/ Endpoint "forceClose" ForceCloseParams

-- | adjudicatorSubscription listens for state changes to the onchain state of
-- the channel. The change can be viewed by subscribers of the PAB instance.
adjudicatorSubscription ::
  ChannelID ->
  Contract PerunEvent AdjudicatorSchema PerunError ()
adjudicatorSubscription cID = do
  let stop = pure $ Right ()
      continue = pure . Left
      loop = loopM $ \cs -> do
        tell []
        adjudicator cID
        getOnChainState cID >>= \case
          Nothing -> logWarn @Text "No onchain state found..." >> stop
          Just cs' -> continue . PChannel . perunState $ cs'
  initial <- getOnChainState cID
  case initial of
    Just cs -> do
      -- TODO: Use and tell events to adjudicator subscriber.
      events <- getAllOnChainEvents cID
      logInfo @String $ unwords ["found initial state for channel:", show cID]
      loop $ PChannel . perunState $ cs
    Nothing -> do
      logInfo @String $ unwords ["found no state for channel:", show cID]
      waitForUpdate cID >>= awaitPromise >>= \case
        Transition cs -> loop $ PChannel . perunState $ cs
        InitialState cs -> loop $ PChannel . perunState $ cs
        ContractEnded -> logWarn @Text "PerunChannel concluded"

adjudicator ::
  (AsPerunError e, AsContractError e) =>
  ChannelID ->
  Contract PerunEvent AdjudicatorSchema e ()
adjudicator cID = do
  updatePromise <- waitForUpdate cID
  let channelUpdate = promiseBind updatePromise $ \case
        ContractEnded -> throwing _SubscriptionError ContractEndedErr
        InitialState initialState -> tell []
        Transition newState -> tell []
      abortChannel = endpoint @"abort" abort
      disputeChannel = endpoint @"dispute" dispute
      closeChannel = endpoint @"close" close
      forceCloseChannel = endpoint @"forceClose" forceClose
  selectList
    [ abortChannel,
      disputeChannel,
      closeChannel,
      forceCloseChannel,
      channelUpdate
    ]

waitForUpdate ::
  (AsPerunError e, AsContractError e) =>
  ChannelID ->
  Contract
    PerunEvent
    s
    e
    ( Promise PerunEvent s e (WaitingResult OnChainState)
    )
waitForUpdate cID = do
  currentState <- getOnChainState cID
  let projectFst = (\(a, (b, _)) -> (a, b))
      success = case currentState of
        Nothing ->
          let addr = channelAddress cID
           in promiseBind (utxoIsProduced addr) $ \ciTx -> do
                logInfo ciTx
                logInfo @String $ unwords ["UTXO produced at:", show addr]
                outRefMaps <-
                  Control.Lens.traverse utxosTxOutTxFromTx ciTx >>= \case
                    -- ChainIndex was not synced properly to retrieve UTXOs for
                    -- ciTx, retry after sync.
                    r | null (concat r) -> retryAfterSync $ Control.Lens.traverse utxosTxOutTxFromTx ciTx
                    outRefMaps -> return outRefMaps
                let produced = getStates (typedChannelValidator cID) (map projectFst . concat $ outRefMaps)
                logInfo @String $ unwords ["Produced", show . length $ produced, "outputs"]
                case chooser produced of
                  Left err -> throwing _PerunError err
                  Right onChainState -> pure $ InitialState onChainState
        Just (OnChainState ocsTxOutRef) ->
          promiseBind (utxoIsSpent (Typed.tyTxOutRefRef ocsTxOutRef)) $ \txn -> do
            outRefMap <-
              map projectFst
                <$> ( utxosTxOutTxFromTx txn >>= \case
                        r | null r -> retryAfterSync $ utxosTxOutTxFromTx txn
                        result -> return result
                    )
            let newStates = getStates (typedChannelValidator cID) outRefMap
            case newStates of
              [] -> pure ContractEnded
              xs -> case chooser xs of
                Left err -> throwing _PerunError err
                Right newState -> pure $ Transition newState
  pure success

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
  Contract PerunEvent s e [ChannelEvent]
getAllOnChainEvents cid = do
  syncToCurrentSlot
  txpoolForChannel cid >>= handlePastEvents
  where
    handlePastEvents (ChannelTxPool []) = do
      -- The channel was never created.
      return []
    handlePastEvents (ChannelTxPool ctxs) = do
      case generateHistory cid refs of
        Left err -> throwing _SubscriptionError err
        Right hist -> return $ deduceEvents hist

-- | generateHistory creates a history for the given ChannelID using tx from
-- the given TxPool.
generateHistory :: ChannelID -> [ChannelTx] -> Either SubscriptionException ChannelHistory
generateHistory _cid [] = return ChannelNil
generateHistory cid hist =
  let -- channelGenesis should ALWAYS exist when we reach this path, otherwise
      -- the given history does not relate to a Perun channel, or something
      -- else went horribly wrong.
      channelGenesis = do
        genesis <- case filter isChannelGenesis hist of
          [] -> throwError CorruptedChainIndexErr
          [ctx] -> do
            -- We expect the resulting list to have exactly one element. If
            -- there are mutiple channel genesis transactions, we are in the
            -- realm of undefined behaviour.
            return $ mkChannelGenesis ctx
          _else -> throwError CorruptedChainIndexErr
   in traceChannelHistory genesis . filter (isNotThisTx genesis) $ hist
  where
    resolveDatum (citx, TxOutRef _ idx, _) = do
      output <- case citx ^. citxOutputs of
        InvalidTx -> throwError CorruptedChainIndexErr
        ValidTx os -> Right $ os !! fromIntegral idx
      datumFromTx (citx, output)
    isChannelGenesis (ChannelTx _ Nothing (Just (_ref, channelDatum))) = isRight channelDatum
    isChannelGenesis (ChannelTx _ i o) = False
    traceChannelHistory ::
      ChannelTxFirst ->
      [ChannelTx] ->
      Either SubscriptionException ChannelHistory
    traceChannelHistory genesisTx@(ChannelTx gCitx _ (_, genesisDatum)) txPool =
      ChannelCreation genesisDatum <$> go genesisTx txPool
      where
        ourValidator = channelValidator cid
        go _ [] = do
          -- No transactions in tx pool left, end of ChannelHistory.
          return ChannelNil
        go ctx txPool = do
          next <- case find (isNextTx ctx) txPool of
            Nothing -> do
              -- If we do not find a successor TX in the txPool AND
              -- have things left in the pool, something is not correct
              -- with the given history, we will abort here.
              throwError DivergentChannelHistories
            Just tx -> return tx
          case next of
            ChannelTx nCitx (Just (iRef, action, iChannelDatum)) (Just (oRef, oChannelDatum)) -> do
              -- We have a channel update. Channel makes a step.
              ChannelStep action <$> go next (filter (isNotThisTx nCitx) txPool)
            ChannelTx nCitx (Just (iRef, action, iChannelDatum)) Nothing -> do
              -- We have a channel close. The transaction closing the channel
              -- has to reference the same state as the last state in the
              -- previous transaction.
              let isSameChannelDatum = do
                    cd <- ctx ^. channelTxOutRef . _Just . _2
                    return $ cd == iChannelDatum
              -- Throw an error if the channelstates do not match.
              unless (fromMaybe False isSameChannelDatum) $ throwError CorruptedChainIndexErr
              ChannelConclude action <$> go next (filter (isNotThisTx nCitx) txPool)
            -- nCitx has only a single ChannelDatum. Either the channel was
            -- concluded or we reached the local end of the ChannelHistory.
            ChannelTx nCitx Nothing (Just _) -> do
              -- This is not possible, since the txPool should NOT contain the
              -- start of the channel when we reach this point.
              throwError CorruptedChainIndexErr
            _else -> do
              -- There is no next transaction but we still have txs left in the
              -- txPool. This is not possible, since the txPool should NOT
              -- contain the start of the channel when we reach this point.
              throwError DivergentChannelHistories -- TODO: Use correct error here.
    isNextTx :: ChannelTx -> ChannelTx -> Bool
    isNextTx (ChannelTx citx _ o) (ChannelTx nCitx ni _) =
      let isNextTx' = do
            ref <- fst <$> o
            nRef <- fst <$> ni
            return $ ref == nRef
       in fromMaybe False isNextTx'
    isNotThisTx citx pCitx = (citx ^. channelcitx) /= (pCitx ^. channelcitx)

-- | ChannelHistory describes the on-chain history of a Perun channel.
data ChannelHistory
  = ChannelCreation !ChannelDatum !ChannelHistory
  | ChannelStep !ChannelAction !ChannelDatum !ChannelHistory
  | ChannelConclude !ChannelAction !ChannelHistory
  | ChannelNil
  deriving (Show)

deduceEvents :: ChannelHistory -> [ChannelEvent]
deduceEvents hist = go Nothing hist
  where
    go _ ChannelNil = []
    go _ (ChannelCreation d h) = Created d : go (Just d) h
    go _ (ChannelStep Abort d h) = Concluded d : go (Just d) h
    go _ (ChannelStep ForceClose d h) = Concluded d : go (Just d) h
    go _ (ChannelStep (MkClose _) d h) = Concluded d : go (Just d) h
    go (Just d') (ChannelStep Fund d h) = mkDepositList d d' ++ go (Just d) h
    go _ (ChannelStep (MkDispute _) d h) = Disputed d : go (Just d) h
    -- We currently expect every participant to deposit for himself. The
    -- resulting event list for deposits will always contain a single
    -- element as long as this restriction is in place.
    mkDepositList d d' =
      let newFunding = d ^. funding
          prevFunding = d' ^. funding
          lenPrevFunding = length prevFunding
       in [Deposited d' (fromIntegral lenPrevFunding) (drop lenPrevFunding newFunding)]

-- | datumFromTx returns the ChannelDatums for the given ChannelID contained
-- in the given ChainIndexTx.
datumFromTx ::
  (ChainIndexTx, ChainIndexTxOut) ->
  Either SubscriptionException ChannelDatum
datumFromTx (ciTx, ciTxOut) = do
  case citoDatum ciTxOut of
    NoOutputDatum -> throwError $ DatumErr NoOutputDatumErr
    OutputDatumHash h -> case Map.lookup h (ciTx ^. citxData) of
      Nothing -> throwError CorruptedChainIndexErr -- TODO: Use correct error type.
      Just d -> channelDatumFromDatum d
    OutputDatum d -> channelDatumFromDatum d

channelDatumFromDatum ::
  Datum ->
  Either SubscriptionException ChannelDatum
channelDatumFromDatum (Datum b) = case PlutusTx.fromBuiltinData b of
  Nothing -> throwError CorruptedChainIndexErr -- TODO: Use correct error type.
  Just d@ChannelDatum {} -> return d

getOnChainState ::
  (AsPerunError e, AsContractError e) =>
  ChannelID ->
  Contract PerunEvent s e (Maybe OnChainState)
getOnChainState cID = do
  utxoTx <- utxosAt $ channelAddress cID
  let states = getStates (typedChannelValidator cID) (Map.toList utxoTx)
  case states of
    [] -> return Nothing
    _else -> case chooser states of
      Left err -> throwing _PerunError err
      Right state -> pure $ Just state

-- | chooser is responsible for resolving the current contractstate from a list
-- of UTXO's. Since there might be more than one UTXO available at a given
-- scriptaddress, a chooser has to be able to determine which of those states
-- is up to date and correct.
--
-- TODO: Implement a chooser which allows watching for the ThreadToken.
chooser :: [OnChainState] -> Either PerunError OnChainState
chooser [ocs] = Right ocs
chooser ocs =
  Left $
    SubscriptionError . InvalidOnchainStatesForChooserErr $
      pack . unwords $
        [ "Number of OnChainStates",
          show . length $ ocs
        ]

getStates :: Scripts.TypedValidator ChannelTypes -> [(TxOutRef, L.ChainIndexTxOut)] -> [OnChainState]
getStates si refMap =
  flip mapMaybe refMap $ \(txOutRef, ciTxOut) -> do
    let txOut = toTxOut ciTxOut
    datum <- ciTxOut ^? ciTxOutScriptDatum . _2 . _Just
    ocsTxOutRef <- either (const Nothing) Just $ Typed.typeScriptTxOutRef si txOutRef txOut datum
    pure $ OnChainState ocsTxOutRef

-- | The result of waiting for a contract update.
data WaitingResult s
  = ContractEnded
  | InitialState !s
  | Transition !s
