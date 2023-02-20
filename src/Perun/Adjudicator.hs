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
        traceChannelHistory genesis . filter (isNotThisTx genesis) $ hist
   in channelGenesis
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
    traceChannelHistory genesisTx txPool =
      ChannelCreation genesisDatum <$> go genesisCitx genesisTxOutRef genesisDatum txPool
      where
        ourValidator = channelValidator cid
        go _ _ _ [] = do
          -- No transactions in tx pool left, end of ChannelHistory.
          return ChannelNil
        go citx txOutRef d txPool = do
          next <- case find (isNextTx (citx, txOutRef)) txPool of
            Nothing -> do
              -- If we do not find a successor TX in the txPool AND
              -- have things left in the pool, something is not correct
              -- with the given history, we will abort here.
              throwError DivergentChannelHistories
            Just tx -> return tx
          case next of
            (nCitx, nTxOutRef, 2, Right nD) -> do
              -- nCitx contains two ChannelDatums, which means we reached a
              -- point in its history, where the channel was progressed
              -- on-chain. We can safely assume, that the channel is still
              -- alive afterwards. So we continue tracing its history.
              --
              -- TODO: Make sure both datums are connected to channel UTXOs.
              let inputsWithThisChannel = filterForValidatorAndChannelDatum ourValidator d nCitx
              r <- case inputsWithThisChannel of
                [TxIn _ (Just (ConsumeScriptAddress _v r _rd))] ->
                  return . fromBuiltinData . getRedeemer $ r
                _else -> do
                  -- Multiple UTXOs with our ChannelDatum cannot exist,
                  -- something is wrong, thus we abort here.
                  throwError CorruptedChainIndexErr
              let txPool' = filter (isNotThisTx nCitx) txPool
              case r of
                Just Fund -> ChannelStep Fund nD <$> go nCitx nTxOutRef nD txPool'
                Just dis@(MkDispute _) -> ChannelStep dis nD <$> go nCitx nTxOutRef nD txPool'
                _else -> throwError CorruptedChainIndexErr

            -- nCitx has only a single ChannelDatum. Either the channel was
            -- concluded or we reached the local end of the ChannelHistory.
            (nCitx, _nTxOutRef, 1, Left (DatumErr NoOutputDatumErr)) -> do
              -- The transaction referenced by the given output datum does not
              -- contain a UTXO with a ChannelDatum as an output. It has to be
              -- an input UTXO containing the last ChannelState we already know
              -- about (`d`).
              let inputsWithThisChannel = filterForValidatorAndChannelDatum ourValidator d nCitx
              r <- case inputsWithThisChannel of
                [TxIn _ (Just (ConsumeScriptAddress _v r _rd))] ->
                  return . fromBuiltinData . getRedeemer $ r
                _else -> throwError CorruptedChainIndexErr
              case r of
                Just c@(MkClose _) -> return $ ChannelConclude c ChannelNil
                Just c@ForceClose -> return $ ChannelConclude c ChannelNil
                Just Abort -> return $ ChannelConclude Abort ChannelNil
                Nothing -> do
                  -- No redeemer could be parsed, this should be impossible,
                  -- because we have a valid PerunScript in our input together
                  -- with a valid state.
                  throwError CorruptedChainIndexErr
                _else -> do
                  -- The channel was progressed for all other cases, stop the
                  -- history here.
                  -- TODO: Make this a proper recursion by using a
                  -- (Maybe ChannelDatum) for calling the recursion.
                  return ChannelNil
            (_nCitx, _nTxOutRef, 1, Right _nD) -> do
              -- The transaction referenced by the given UTXO contains a single
              -- ChannelDatum in its OUTPUT. This is not possible, since the
              -- txPool should NOT contain the start of the channel when we
              -- reach this point.
              throwError CorruptedChainIndexErr
            _else -> do
              -- The next transaction does not satisfy the invariants described
              -- in `isUniqueHistory`.
              throwError DivergentChannelHistories -- TODO: Use correct error here.
    isNextTx ::
      (ChainIndexTx, TxOutRef) ->
      (ChainIndexTx, TxOutRef, Int, Either SubscriptionException ChannelDatum) ->
      Bool
    isNextTx (_, txOutRef) (nCitx, _, _, _) = any (\(TxIn ref _) -> txOutRef == ref) $ nCitx ^. citxInputs
    isNotThisTx citx pCitx = (citx ^. channelcitx) /= (pCitx ^. channelcitx)
    filterForValidatorAndChannelDatum ourValidator d =
      filter
        ( \case
            TxIn _ (Just (ConsumeScriptAddress (Versioned otherValidator _) _r rd)) ->
              case channelDatumFromDatum rd of
                Right d' -> d == d' && ourValidator == otherValidator
                _else -> False
            _else -> False
        )
        . _citxInputs

-- | ChannelHistory describes the on-chain history of a Perun channel.
data ChannelHistory
  = ChannelCreation !ChannelDatum !ChannelHistory
  | ChannelStep !ChannelAction !ChannelDatum !ChannelHistory
  | ChannelConclude !ChannelAction !ChannelHistory
  | ChannelNil
  deriving (Show)

deduceEvents :: ChannelHistory -> [ChannelEvent]
deduceEvents = undefined

-- | isUniqueHistory checks that the given pool of transactions describes a
-- unique history of some channel. This is true iff the following conditions
-- hold:
-- 1. At maximum two UNIQUE ChainIndexTxs with a single ChannelDatum exist.
-- 2. All other ChainIndexTxs contain exactly two ChannelDatums, where one is
--    for the input UTXO and one for the output UTXO.
-- 3. Implied by 1., 2. and the UTXO model but checked explicitely as a sanity
-- check: ZERO single ChannelDatum txs exist => Zero multi ChannelDatum txs
isUniqueHistory :: [(ChainIndexTx, TxOutRef, Int)] -> Bool
isUniqueHistory refs =
  let uniqueRefs = dedup refs
      (singleDatumTxs, multiDatumTxs) = split (\(_citx, _txoref, n) -> n == 1) uniqueRefs
      numOfSingleDatumTxs = length singleDatumTxs
      numOfTwoDatumTxs = length . filter (\(_, _, n) -> n == 2) $ multiDatumTxs
   in ( -- 1.
        (numOfSingleDatumTxs == 1 || numOfSingleDatumTxs == 2)
          -- 2.
          && numOfTwoDatumTxs == length multiDatumTxs
      )
        -- 3.
        || (null singleDatumTxs && null multiDatumTxs)

-- | split splits the given list using the condition into two lists, where the
-- left result identifies all entries for which the condition was true, while
-- the right result identifies all entries for which the condition was false.
split :: (a -> Bool) -> [a] -> ([a], [a])
split cond as = go as ([], [])
  where
    go [] res = res
    go (x : xs) (ls, rs) =
      if cond x
        then go xs (x : ls, rs)
        else go xs (ls, x : rs)

-- | dedup removes duplicates from a list of triplets, where the first entry is
-- regarded as the comparative argument.
dedup :: Eq a => [(a, b, c)] -> [(a, b, c)]
dedup [] = []
dedup (oref : orems) = go oref orems []
  where
    go :: Eq a => (a, b, c) -> [(a, b, c)] -> [(a, b, c)] -> [(a, b, c)]
    go ref [] res = reverse (ref : res)
    go ref@(citx, _, _) (nextRef@(citx', _, _) : rems) res =
      if citx == citx'
        then go nextRef rems res
        else go nextRef rems (ref : res)

filterForChannelOutputs ::
  (AsPerunError e, AsContractError e) =>
  [TxOutRef] ->
  Contract PerunEvent s e [(ChainIndexTx, TxOutRef, Int)]
filterForChannelOutputs txoRefs = do
  let resolveTx = txFromTxId . txOutRefId
  txs <- catMaybes <$> mapM (\r -> fmap (,r) <$> resolveTx r) txoRefs
  let countDatums (citx, _) =
        let toChannelDatum (Datum d) = PlutusTx.fromBuiltinData @ChannelDatum d
            datums = catMaybes . Map.elems . Map.map toChannelDatum $ citx ^. citxData
            numOfDatums = length datums
         in numOfDatums
  return $ filter (\(_, _, i) -> i /= 0) . zipWith (\(a, b) c -> (a, b, c)) txs $ map countDatums txs

-- | translateEventsFromHistory translates a list of transaction outputs to
-- ChannelEvents.
translateEventsFromHistory ::
  (AsPerunError e, AsContractError e) =>
  [(ChainIndexTx, ChainIndexTxOut)] ->
  Contract PerunEvent s e [ChannelEvent]
translateEventsFromHistory = para ralg
  where
    ralg Nil = return []
    ralg (Cons _ ([], _)) = return []
    ralg (Cons curTx (nextTx : _, actions)) = do
      a <- datumFromTx' curTx
      b <- datumFromTx' nextTx
      let event = resolveEvent a b
      (event ++) <$> actions

    datumFromTx' ::
      (AsPerunError e, AsContractError e) =>
      (ChainIndexTx, ChainIndexTxOut) ->
      Contract PerunEvent s e ChannelDatum
    datumFromTx' (ciTx, ciTxOut) = do
      case citoDatum ciTxOut of
        NoOutputDatum -> throwing _SubscriptionError CorruptedChainIndexErr
        OutputDatumHash h -> case Map.lookup h (ciTx ^. citxData) of
          Nothing -> throwing _SubscriptionError CorruptedChainIndexErr
          Just d -> channelDatumFromDatum' d
        OutputDatum d -> channelDatumFromDatum' d

    channelDatumFromDatum' ::
      (AsPerunError e, AsContractError e) =>
      Datum ->
      Contract PerunEvent s e ChannelDatum
    channelDatumFromDatum' (Datum b) = case PlutusTx.fromBuiltinData b of
      Nothing -> throwing _FindChannelError WrongDatumTypeError
      Just d@ChannelDatum {} -> return d

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
