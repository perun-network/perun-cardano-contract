{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Perun.Offchain where

import Control.Monad as CM hiding (fmap)
import Control.Monad.Error.Lens
import Data.Aeson (FromJSON, ToJSON)
import Data.List (genericSplitAt)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Monoid (Last (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import qualified Ledger.Constraints as Constraints
import Ledger.Constraints.OffChain ()
import qualified Ledger.Scripts as S
import Ledger.Value (AssetClass (..), valueOf)
import Perun.Error
import Perun.Onchain
import Plutus.ChainIndex.Types hiding (ChainIndexTxOut)
import Plutus.Contract
import Plutus.Contract.Oracle (SignedMessage (..))
import qualified PlutusTx
import PlutusTx.Prelude hiding (unless)
import Schema (ToSchema)
import Text.Hex (encodeHex)
import Text.Printf (printf)
import qualified Prelude as P

--
--
-- OFF-CHAIN PART
--
--

--
-- Parameters for Endpoints, that can then be invoked
--

data OpenParams = OpenParams
  { spChannelId :: !ChannelID,
    spSigningPKs :: ![PaymentPubKey],
    spPaymentPKs :: ![PaymentPubKeyHash],
    spBalances :: ![Integer],
    spTimeLock :: !Integer,
    spNonce :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
  deriving stock (P.Eq, P.Show)

data FundParams = FundParams
  { fpChannelId :: !ChannelID,
    fpChannelToken :: !AssetClass,
    fpIndex :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
  deriving stock (P.Eq, P.Show)

data AbortParams = AbortParams
  { apChannelId :: !ChannelID,
    apChannelToken :: !AssetClass
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
  deriving stock (P.Eq, P.Show)

newtype AllSignedStates = AllSignedStates
  { allSignedStates :: [SignedMessage ChannelState]
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

deriving instance ToSchema AllSignedStates

data DisputeParams = DisputeParams
  { dpChannelId :: !ChannelID,
    dpChannelToken :: !AssetClass,
    dpSigningPKs :: ![PaymentPubKey],
    dpSignedState :: !AllSignedStates
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
  deriving stock (P.Eq, P.Show)

data CloseParams = CloseParams
  { cpChannelId :: !ChannelID,
    cpChannelToken :: !AssetClass,
    cpSigningPKs :: ![PaymentPubKey],
    cpSignedState :: !AllSignedStates
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
  deriving stock (P.Eq, P.Show)

data ForceCloseParams = ForceCloseParams
  { fcpChannelId :: !ChannelID,
    fcpChannelToken :: !AssetClass
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
  deriving stock (P.Eq, P.Show)

type ChannelSchema =
  Endpoint "start" OpenParams
    .\/ Endpoint "fund" FundParams
    .\/ Endpoint "abort" AbortParams
    .\/ Endpoint "open" OpenParams
    .\/ Endpoint "dispute" DisputeParams
    .\/ Endpoint "close" CloseParams
    .\/ Endpoint "forceClose" ForceCloseParams

--
-- open channel
--

start ::
  (AsPerunError e, AsContractError e) =>
  OpenParams ->
  Contract TokenState s e ()
start OpenParams {..} = do
  unless (all isLegalOutValue spBalances) . throwing_ $ _InsufficientMinimumAdaBalanceError
  now <- currentTime
  utxos <- ownUtxos
  ref <- case Map.toList utxos of
    (oref, _) : _ -> return oref
    [] -> throwing _FindChannelError NoUTXOsError
  let symbol = channelTokenSymbol ref
      hash = channelHash spChannelId
      name = channelTokenName hash
      token = ChannelToken symbol name ref
      c =
        Channel
          { pTimeLock = spTimeLock,
            pSigningPKs = spSigningPKs,
            pPaymentPKs = spPaymentPKs,
            pNonce = spNonce
          }
      s =
        ChannelState
          { channelId = spChannelId,
            balances = spBalances,
            version = 0,
            final = False
          }
      d =
        ChannelDatum
          { channelParameters = c,
            channelToken = token,
            state = s,
            time = now + 1,
            funding = head spBalances : tail (map (const 0) spBalances),
            funded = False,
            disputed = False
          }
      tokenVal = channelTokenValue symbol name
      v = Ada.lovelaceValueOf (head spBalances) <> tokenVal
      lookups = Constraints.typedValidatorLookups (typedChannelValidator spChannelId) P.<> Constraints.plutusV2OtherScript (channelValidator spChannelId) P.<> Constraints.mintingPolicy (mkVersionedChannelTokenMintinPolicy ref) P.<> Constraints.unspentOutputs utxos
      tx = Constraints.mustPayToTheScript d v <> Constraints.mustMintValueWithRedeemer (Redeemer (PlutusTx.toBuiltinData (hash, Perun.Onchain.Mint))) tokenVal <> Constraints.mustSpendPubKeyOutput ref
  unless (spChannelId == getChannelId c) . throwing_ $ _ChannelIdMismatchError
  ledgerTx <- submitTxConstraintsWith lookups tx
  void . awaitTxConfirmed $ getCardanoTxId ledgerTx
  tellToken token
  logInfo @P.String $ printf "Started funding for channel %s with parameters %s and value %s" (P.show spChannelId) (P.show c) (P.show v)

fund ::
  (AsPerunError e, AsContractError e) =>
  FundParams ->
  Contract w s e ()
fund FundParams {..} = do
  (oref, o, d@ChannelDatum {..}) <- findChannelWithSync' fpChannelId fpChannelToken
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  -- TODO add more checks before funding
  unless (all isLegalOutValue (balances state)) . throwing_ $ _InsufficientMinimumAdaBalanceError
  when funded . throwing_ $ _RedundantFundError
  let -- TODO avoid using list-indexing with !!
      --      try to use fixed-sized arrays instead
      newFunding = addFunding (balances state !! fpIndex) fpIndex funding
      newDatum =
        d
          { funding = newFunding,
            funded = newFunding == balances state
          }
      v = Ada.lovelaceValueOf (sum newFunding) <> channelTokenValue' channelToken
      r = Redeemer (PlutusTx.toBuiltinData Fund)
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator fpChannelId)
          P.<> Constraints.plutusV2OtherScript (channelValidator fpChannelId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        Constraints.mustPayToTheScript newDatum v
          <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void . awaitTxConfirmed $ getCardanoTxId ledgerTx
  -- TODO avoid using list-indexing with !!
  --      try to use fixed-sized arrays instead
  logInfo @P.String $ printf "Funded %d Lovelace for party %d on channel %s" (balances state !! fpIndex) fpIndex (P.show fpChannelId)

abort ::
  (AsPerunError e, AsContractError e) =>
  AbortParams ->
  Contract w s e ()
abort (AbortParams cId ct) = do
  (oref, o, d@ChannelDatum {..}) <- findChannelWithSync' cId ct
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  -- TODO: This still gives the potential to lock a channel?
  unless (all isLegalOutValue funding) . throwing_ $ _InsufficientMinimumAdaBalanceError
  when funded . throwing_ $ _AlreadyFundedAbortError
  let r = Redeemer (PlutusTx.toBuiltinData Abort)
      hash = channelHash cId
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator cId)
          P.<> Constraints.plutusV2OtherScript (channelValidator cId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
          P.<> (Constraints.mintingPolicy (mkVersionedChannelTokenMintinPolicy (ctTxOutRef channelToken)))
      tx =
        mconcat (zipWith Constraints.mustPayToPubKey (pPaymentPKs channelParameters) (map Ada.lovelaceValueOf funding))
          <> Constraints.mustSpendScriptOutput oref r
          <> (Constraints.mustMintValueWithRedeemer (Redeemer (PlutusTx.toBuiltinData (hash, Perun.Onchain.Burn))) (inv $ channelTokenValue' channelToken))
  ledgerTx <- submitTxConstraintsWith lookups tx
  void . awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $
    printf
      "aborted channel %s with parameters %s. The funding was: %s"
      (P.show cId)
      (P.show channelParameters)
      (P.show funding)

-- alternative way to open a channel "at once" with all funding in place
open ::
  (AsPerunError e, AsContractError e) =>
  OpenParams ->
  Contract TokenState s e ()
open OpenParams {..} = do
  unless (all isLegalOutValue spBalances) . throwing_ $ _InsufficientMinimumAdaBalanceError
  t <- currentTime
  utxos <- ownUtxos
  ref <- case Map.toList utxos of
    (oref, _) : _ -> return oref
    [] -> throwing _FindChannelError NoUTXOsError
  logInfo @P.String $ printf "Opening out-ref: %s" (P.show ref)
  let symbol = channelTokenSymbol ref
      hash = channelHash spChannelId
      name = channelTokenName hash
      token = ChannelToken symbol name ref
      c =
        Channel
          { pTimeLock = spTimeLock,
            pSigningPKs = spSigningPKs,
            pPaymentPKs = spPaymentPKs,
            pNonce = spNonce
          }
      s =
        ChannelState
          { channelId = spChannelId,
            balances = spBalances,
            version = 0,
            final = False
          }
      d =
        ChannelDatum
          { channelParameters = c,
            channelToken = token,
            state = s,
            time = t,
            funding = [],
            funded = True,
            disputed = False
          }
      tokenVal = channelTokenValue symbol name
      v = Ada.lovelaceValueOf (sum spBalances) <> tokenVal
      lookups = (Constraints.typedValidatorLookups (typedChannelValidator spChannelId)) P.<> Constraints.plutusV2OtherScript (channelValidator spChannelId) P.<> (Constraints.mintingPolicy (mkVersionedChannelTokenMintinPolicy ref)) P.<> Constraints.unspentOutputs utxos
      tx = (Constraints.mustPayToTheScript d v) <> (Constraints.mustMintValueWithRedeemer (Redeemer (PlutusTx.toBuiltinData (hash, Perun.Onchain.Mint))) tokenVal) <> Constraints.mustSpendPubKeyOutput ref
  unless (spChannelId == getChannelId c) . throwing_ $ _ChannelIdMismatchError
  ledgerTx <- submitTxConstraintsWith lookups tx
  void . awaitTxConfirmed $ getCardanoTxId ledgerTx
  tellToken token
  logInfo @P.String $ printf "Opened channel %s with parameters %s and value %s" (P.show spChannelId) (P.show c) (P.show v)

--
-- dispute logic
--

dispute ::
  (AsPerunError e, AsContractError e) => DisputeParams -> Contract w s e ()
dispute (DisputeParams dpChannelId ct keys ast) = do
  let signedState = compressSignatures ast
      dState = extractVerifiedState signedState keys
  unless ((channelId dState) == dpChannelId) . throwing_ $ _ChannelIdMismatchError
  now <- currentTime
  (oref, o, d@ChannelDatum {..}) <- findChannelWithSync' dpChannelId ct
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  unless (all isLegalOutValue (balances dState)) . throwing_ $ _InsufficientMinimumAdaBalanceError
  unless (isValidStateTransition state dState) . throwing_ $ _InvalidStateTransitionError
  unless funded . throwing_ $ _PrematureDisputeError
  let newDatum =
        d
          { state = dState,
            time = now + 1,
            disputed = True
          }
      v = (Ada.lovelaceValueOf . sum $ balances state) <> channelTokenValue' channelToken
      r = Redeemer . PlutusTx.toBuiltinData $ MkDispute signedState
      -- Notes on time-handling:
      --  * currentTime returns the last POSIX time belonging to the current slot (which means it ends with "999" for the current SlotConfig)
      --  * In theory, one would assume that:
      --    * now = currentTime
      --    * mustValidateIn Constraint = [now, _]
      --    * timestamp in outputDatum = now
      --    => the on-chain validator would evaluate "timestamp `member` validTimeRange" to true but THIS IS NOT THE CASE!
      --  * We observe that if we set the timestamp to now + 1 (which is exactly the beginning of the next slot), the validator actually
      --    evaluates "timestamp `member` validTimeRange" to true!
      --  * Consequently, we assume that somewhere in the PAB, the wallet or the node the conversion between slots and POSIXTime is mishandled.
      --  * We still use this API as we assume it is supposed to be used here and just apply the "timestamp + 1" hack because:
      --    * Handling this appropriately would mean setting the mustValidateIn constraint to "[now - slotLength, _]" but we can not access the
      --      slotLength conveniently
      --    * Adjusting the timestamp by 1 ms is irrelevant for our use case as channels deal with substantially longer timelocks and we can only
      --      assume the timestamp to be accurate to the degree of the length of the valid range when validating on-chain anyways.
      --      (See relative timelock documentation)
      --    * We agreed to go back to this issue if this either gets fixed in the PAB, the wallet or the node, or if our assumptions above turn out
      --      to be invalid
      range = Ledger.interval now (now + defaultValidMsRange)

      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator $ channelId dState)
          P.<> Constraints.plutusV2OtherScript (channelValidator $ channelId dState)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        Constraints.mustPayToTheScript newDatum v
          <> Constraints.mustValidateIn range
          <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void . awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $ printf "made dispute of new state with datum: %s" (P.show newDatum)

syncToCurrentSlot :: (AsPerunError e, AsContractError e) => Contract w s e ()
syncToCurrentSlot = currentPABSlot >>= awaitChainIndexSlot

retryAfterSync :: (AsPerunError e, AsContractError e) => Contract w s e a -> Contract w s e a
retryAfterSync action = syncToCurrentSlot >> action

awaitChainIndexSlot :: (AsPerunError e, AsContractError e) => Slot -> Contract w s e ()
awaitChainIndexSlot targetSlot = do
  chainIndexTip <- getTip
  let chainIndexSlot = getChainIndexSlot chainIndexTip
  when (chainIndexSlot < targetSlot) $ do
    void $ waitNSlots 1
    awaitChainIndexSlot targetSlot
  where
    getChainIndexSlot :: Tip -> Slot
    getChainIndexSlot TipAtGenesis = Slot 0
    getChainIndexSlot (Tip slot _ _) = slot

--
-- close channel
--

close ::
  (AsPerunError e, AsContractError e) =>
  CloseParams ->
  Contract w s e ()
close (CloseParams cId ct keys ast) = do
  let signedState = compressSignatures ast
      s@ChannelState {..} = extractVerifiedState signedState keys
  unless (channelId == cId) . throwing_ $ _ChannelIdMismatchError
  unless final . throwing_ $ _CloseOnNonFinalChannelError
  (oref, o, ChannelDatum {..}) <- findChannelWithSync' channelId ct
  unless (all isLegalOutValue balances) . throwing_ $ _InsufficientMinimumAdaBalanceError
  unless (isValidStateTransition state s) . throwing_ $ _InvalidStateTransitionError
  unless funded . throwing_ $ _CloseOnNonFundedChannelError
  let r = Redeemer . PlutusTx.toBuiltinData $ MkClose signedState
      hash = channelHash cId
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator channelId)
          P.<> Constraints.plutusV2OtherScript (channelValidator channelId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
          P.<> (Constraints.mintingPolicy (mkVersionedChannelTokenMintinPolicy (ctTxOutRef channelToken)))
      tx =
        mconcat (zipWith Constraints.mustPayToPubKey (pPaymentPKs channelParameters) (map Ada.lovelaceValueOf balances))
          <> Constraints.mustSpendScriptOutput oref r
          <> (Constraints.mustMintValueWithRedeemer (Redeemer (PlutusTx.toBuiltinData (hash, Perun.Onchain.Burn))) (inv $ channelTokenValue' channelToken))
  ledgerTx <- submitTxConstraintsWith lookups tx
  void . awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $
    printf
      "closed channel %s with params %s. The final balances are: %s"
      (P.show channelId)
      (P.show channelParameters)
      (P.show balances)

forceClose ::
  (AsPerunError e, AsContractError e) =>
  ForceCloseParams ->
  Contract w s e ()
forceClose (ForceCloseParams cId ct) = do
  (oref, o, d@ChannelDatum {..}) <- findChannelWithSync' cId ct
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  unless (all isLegalOutValue (balances state)) . throwing_ $ _InsufficientMinimumAdaBalanceError
  unless disputed . throwing_ $ _ForceCloseOnNonDisputedChannelError
  unless funded . throwing_ $ _ForceCloseOnNonFundedChannelError
  let r = Redeemer (PlutusTx.toBuiltinData ForceClose)
      hash = channelHash cId
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator cId)
          P.<> Constraints.plutusV2OtherScript (channelValidator cId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
          P.<> (Constraints.mintingPolicy (mkVersionedChannelTokenMintinPolicy (ctTxOutRef channelToken)))
      tx =
        mconcat (zipWith Constraints.mustPayToPubKey (pPaymentPKs channelParameters) (map Ada.lovelaceValueOf (balances state)))
          <> Constraints.mustValidateIn (Ledger.from $ earliestFirstCloseTime time (pTimeLock channelParameters))
          <> Constraints.mustSpendScriptOutput oref r
          <> (Constraints.mustMintValueWithRedeemer (Redeemer (PlutusTx.toBuiltinData (hash, Perun.Onchain.Burn))) (inv $ channelTokenValue' channelToken))
  ledgerTx <- submitTxConstraintsWith lookups tx
  void . awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $
    printf
      "force closed channel %s with parameters %s. The final balances are: %s"
      (P.show cId)
      (P.show channelParameters)
      (P.show $ balances state)

-- | Calculates the earliest time a channel can be force-closed for given parameters.
earliestFirstCloseTime :: POSIXTime -> Integer -> POSIXTime
earliestFirstCloseTime timeStamp timeLock = timeStamp + POSIXTime timeLock

-- | findChannelWithSync is an optimistic findChannel handler. It first tries
-- to find a channel and if not successful, retries it again AFTER making sure
-- to synchronize with the chainindex to its current slot.
findChannelWithSync ::
  (AsPerunError e, AsContractError e) =>
  ChannelID ->
  Contract w s e (TxOutRef, ChainIndexTxOut, ChannelDatum)
findChannelWithSync cid =
  handleError errorHandler $ findChannel cid
  where
    errorHandler (FindChannelError _) = do
      logWarn @Text "Optimistic findChannel not succeeded. Trying again after syncing PAB with Chainindex."
      retryAfterSync $ findChannel cid
    errorHandler err = throwing _PerunError err

-- | findChannelWithSync' is an optimistic findChannel handler. It first tries
-- to find a valid channel for the given channel id and thread token and if not successful,
-- retries it again AFTER making sure to synchronize with the chainindex to its current slot.
findChannelWithSync' ::
  (AsPerunError e, AsContractError e) =>
  ChannelID ->
  AssetClass ->
  Contract w s e (TxOutRef, ChainIndexTxOut, ChannelDatum)
findChannelWithSync' cid ct =
  handleError errorHandler $ findChannel cid
  where
    errorHandler (FindChannelError _) = do
      logWarn @Text "Optimistic findChannel not succeeded. Trying again after syncing PAB with Chainindex."
      retryAfterSync $ findChannel' cid ct
    errorHandler err = throwing _PerunError err

getChannelId :: Channel -> ChannelID
getChannelId channel = ChannelID . S.dataHash $ PlutusTx.toBuiltinData channel

findChannel ::
  (AsPerunError e, AsContractError e) =>
  ChannelID ->
  Contract w s e (TxOutRef, ChainIndexTxOut, ChannelDatum)
findChannel cID = do
  utxos <- utxosAt $ channelAddress cID
  case Map.toList utxos of
    [(oref, o)] -> case _ciTxOutScriptDatum o of
      (_, Just (Datum e)) -> case PlutusTx.fromBuiltinData e of
        Nothing -> throwing _FindChannelError WrongDatumTypeError
        Just d@ChannelDatum {} -> return (oref, o, d)
      _otherwise -> throwing _FindChannelError DatumMissingError
    [] -> throwing _FindChannelError NoUTXOsError
    _utxos -> throwing _FindChannelError UnexpectedNumberOfUTXOsError

-- | findChannel' searches the utxos at the channel address (parameterized by the channel id)
-- for an utxo that has:
-- 1. the given ThreadToken named in the utxo's datum and attached to the utxo's value.
-- 2. the ThreadToken's TokenName is equal to the ValidatorHash of the channel validator for the given channel id.
-- 3. the channelParameters in the utxo's evaluating to the given channel id.
findChannel' ::
  (AsPerunError e, AsContractError e) =>
  ChannelID ->
  AssetClass ->
  Contract w s e (TxOutRef, ChainIndexTxOut, ChannelDatum)
findChannel' cID ct = do
  utxos <- utxosAt $ channelAddress cID
  let utxoList = Map.toList utxos
  filterChannelUtxos utxoList
  where
    filterChannelUtxos :: (AsPerunError e, AsContractError e) => [(TxOutRef, ChainIndexTxOut)] -> Contract w s e (TxOutRef, ChainIndexTxOut, ChannelDatum)
    filterChannelUtxos [] = throwing _FindChannelError NoUTXOsError
    filterChannelUtxos ((oref, o) : xs) = do
      let AssetClass (s, tn) = ct
      if valueOf (_ciTxOutValue o) s tn == 1
        then -- Note: If any check fails at this point, we can safely throw an error
        -- because we know that the utxo at this address carries the ThreadToken.AbortCase
        -- Therefore, no other utxo can carry the ThreadToken, as it is an NFT.
        case _ciTxOutScriptDatum o of
          (_, Just (Datum e)) -> case PlutusTx.fromBuiltinData e of
            Nothing -> throwing _FindChannelError WrongDatumTypeError
            Just d@ChannelDatum {..} -> if (channelTokenAsset channelToken) == ct && cID == getChannelId channelParameters && (channelTokenName (channelHash cID)) == tn then return (oref, o, d) else throwing _FindChannelError InvalidDatumError
          _otherwise -> throwing _FindChannelError DatumMissingError
        else filterChannelUtxos xs

addFunding :: Integer -> Integer -> [Integer] -> [Integer]
addFunding amount idx f =
  let (s, e) = genericSplitAt idx f
   in s ++ amount : P.drop 1 e

type TokenState = Last ChannelToken

tellToken :: (AsPerunError e, AsContractError e) => ChannelToken -> Contract TokenState s e ()
tellToken = tell . Last . Just

--
-- Top-level contract, exposing all endpoints.
--
contract :: Contract TokenState ChannelSchema PerunError ()
contract =
  selectList
    [ start',
      fund',
      abort',
      open',
      dispute',
      close',
      forceClose'
    ]
    >> contract
  where
    start' = endpoint @"start" start
    fund' = endpoint @"fund" fund
    abort' = endpoint @"abort" abort
    open' = endpoint @"open" open
    dispute' = endpoint @"dispute" dispute
    close' = endpoint @"close" close
    forceClose' = endpoint @"forceClose" forceClose

compressSignatures :: AllSignedStates -> SignedState
compressSignatures (AllSignedStates sst) = SignedState (map (\(SignedMessage sig _ _) -> sig) sst) (osmMessageHash (head sst)) (fromJust . PlutusTx.fromBuiltinData . getDatum $ osmDatum (head sst))

-- | isValidChannelStart checks if transaction represented by the given list of inputs,
-- and the given *single* channel output is a valid opening transaction for a channel with
-- the given channel id.
isValidChannelStart :: [TxOutRef] -> (Address, Value, ChannelDatum) -> ChannelID -> Bool
isValidChannelStart inputs (outAddress, outValue, ChannelDatum {..}) cId =
  let ref = ctTxOutRef channelToken
      s = channelTokenSymbol ref
      tn = channelTokenName (channelHash cId)
   in ref `elem` inputs
        && getChannelId channelParameters == cId
        && outAddress == channelAddress cId
        && valueOf outValue s tn == 1
        && channelToken == ChannelToken s tn ref
        && not disputed
        && False -- TODO: Complete these checks!
