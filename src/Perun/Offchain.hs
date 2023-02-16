{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Perun.Offchain where

import Control.Monad as CM hiding (fmap)
import Control.Monad.Error.Lens
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Default
import Data.Either (fromRight)
import Data.List (genericSplitAt)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import qualified Ledger.Constraints as Constraints
import Ledger.Constraints.OffChain ()
import qualified Ledger.Scripts as S
import Perun.Error
import Perun.Onchain
import Plutus.ChainIndex.Types hiding (ChainIndexTxOut)
import Plutus.Contract
import Plutus.Contract.Effects (ChainIndexQuery (..), ChainIndexResponse (TxIdResponse), PABReq (..), _ChainIndexQueryResp)
import Plutus.Contract.Request (pabReq, txoRefsAt)
import Plutus.Contract.Util (loopM)
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Typed
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts
import Plutus.V2.Ledger.Api (BuiltinByteString, ToData (toBuiltinData))
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
    fpIndex :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
  deriving stock (P.Eq, P.Show)

newtype AbortParams = AbortParams ChannelID
  deriving newtype (ToJSON, FromJSON, ToSchema)
  deriving (Generic)
  deriving stock (P.Eq, P.Show)

data DisputeParams = DisputeParams
  { dpSigningPKs :: ![PaymentPubKey],
    dpSignedState :: !SignedState
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
  deriving stock (P.Eq, P.Show)

data CloseParams = CloseParams
  { cpSigningPKs :: ![PaymentPubKey],
    cpSignedState :: !SignedState
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
  deriving stock (P.Eq, P.Show)

newtype ForceCloseParams = ForceCloseParams ChannelID
  deriving newtype (ToJSON, FromJSON, ToSchema)
  deriving (Generic)
  deriving stock (P.Eq, P.Show)

type ChannelSchema =
  Endpoint "start" OpenParams
    .\/ Endpoint "fund" FundParams
    .\/ Endpoint "abort" AbortParams
    .\/ Endpoint "open" OpenParams
    .\/ Endpoint "dispute" DisputeParams
    .\/ Endpoint "close" CloseParams
    .\/ Endpoint "forceClose" ForceCloseParams
    .\/ Endpoint "dummy" ChannelID
    .\/ Endpoint "dummyPayment" PaymentPubKeyHash

--
-- open channel
--

start ::
  (AsPerunError e, AsContractError e) =>
  OpenParams ->
  Contract w s e ()
start OpenParams {..} = do
  unless (all isLegalOutValue spBalances) . throwing_ $ _InsufficientMinimumAdaBalanceError
  now <- currentTime
  let c =
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
            state = s,
            time = now + 1,
            funding = head spBalances : tail (map (const 0) spBalances),
            funded = False,
            disputed = False
          }
      v = Ada.lovelaceValueOf $ head spBalances
      tx = Constraints.mustPayToTheScript d v
  ledgerTx <- submitTxConstraints (typedChannelValidator spChannelId) tx
  void . awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $ printf "Started funding for channel %s with parameters %s and value %s" (P.show spChannelId) (P.show c) (P.show v)

fund ::
  (AsPerunError e, AsContractError e) =>
  FundParams ->
  Contract w s e ()
fund FundParams {..} = do
  (oref, o, d@ChannelDatum {..}) <- findChannelWithSync fpChannelId
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
      v = Ada.lovelaceValueOf (sum newFunding)
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
abort (AbortParams cId) = do
  (oref, o, d@ChannelDatum {..}) <- findChannelWithSync cId
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  -- TODO: This still gives the potential to lock a channel?
  unless (all isLegalOutValue funding) . throwing_ $ _InsufficientMinimumAdaBalanceError
  when funded . throwing_ $ _AlreadyFundedAbortError
  let r = Redeemer (PlutusTx.toBuiltinData Abort)
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator cId)
          P.<> Constraints.plutusV2OtherScript (channelValidator cId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        mconcat (zipWith Constraints.mustPayToPubKey (pPaymentPKs channelParameters) (map Ada.lovelaceValueOf funding))
          <> Constraints.mustSpendScriptOutput oref r
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
  Contract w s e ()
open OpenParams {..} = do
  unless (all isLegalOutValue spBalances) . throwing_ $ _InsufficientMinimumAdaBalanceError
  t <- currentTime
  let c =
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
            state = s,
            time = t,
            funding = [],
            funded = True,
            disputed = False
          }
      v = Ada.lovelaceValueOf $ sum spBalances
      tx = Constraints.mustPayToTheScript d v
  ledgerTx <- submitTxConstraints (typedChannelValidator spChannelId) tx
  void . awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $ printf "Opened channel %s with parameters %s and value %s" (P.show spChannelId) (P.show c) (P.show v)

--
-- dispute logic
--

dispute ::
  (AsPerunError e, AsContractError e) => DisputeParams -> Contract w s e ()
dispute (DisputeParams keys sst) = do
  let dState = extractVerifiedState sst keys
  now <- currentTime
  (oref, o, d@ChannelDatum {..}) <- findChannelWithSync $ channelId dState
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
      v = Ada.lovelaceValueOf . sum $ balances state
      r = Redeemer . PlutusTx.toBuiltinData $ MkDispute sst
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
close (CloseParams keys sst) = do
  let s@ChannelState {..} = extractVerifiedState sst keys
  (oref, o, ChannelDatum {..}) <- findChannelWithSync channelId
  unless (all isLegalOutValue balances) . throwing_ $ _InsufficientMinimumAdaBalanceError
  unless (isValidStateTransition state s) . throwing_ $ _InvalidStateTransitionError
  unless final . throwing_ $ _CloseOnNonFinalChannelError
  unless funded . throwing_ $ _CloseOnNonFundedChannelError
  let r = Redeemer . PlutusTx.toBuiltinData $ MkClose sst
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator channelId)
          P.<> Constraints.plutusV2OtherScript (channelValidator channelId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        mconcat (zipWith Constraints.mustPayToPubKey (pPaymentPKs channelParameters) (map Ada.lovelaceValueOf balances))
          <> Constraints.mustSpendScriptOutput oref r
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
forceClose (ForceCloseParams cId) = do
  (oref, o, d@ChannelDatum {..}) <- findChannelWithSync cId
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  unless (all isLegalOutValue (balances state)) . throwing_ $ _InsufficientMinimumAdaBalanceError
  unless disputed . throwing_ $ _ForceCloseOnNonDisputedChannelError
  unless funded . throwing_ $ _ForceCloseOnNonFundedChannelError
  let r = Redeemer (PlutusTx.toBuiltinData ForceClose)
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator cId)
          P.<> Constraints.plutusV2OtherScript (channelValidator cId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        mconcat (zipWith Constraints.mustPayToPubKey (pPaymentPKs channelParameters) (map Ada.lovelaceValueOf (balances state)))
          <> Constraints.mustValidateIn (Ledger.from $ earliestFirstCloseTime time (pTimeLock channelParameters))
          <> Constraints.mustSpendScriptOutput oref r
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

getChannelId :: Channel -> ChannelID
getChannelId channel = ChannelID . S.dataHash $ toBuiltinData channel

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

addFunding :: Integer -> Integer -> [Integer] -> [Integer]
addFunding amount idx f =
  let (s, e) = genericSplitAt idx f
   in s ++ amount : P.drop 1 e

--
-- Top-level contract, exposing all endpoints.
--
contract :: Contract () ChannelSchema PerunError ()
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
