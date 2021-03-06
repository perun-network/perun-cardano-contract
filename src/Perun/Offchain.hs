{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Perun.Offchain where

import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.List (genericSplitAt)
import Data.Map as Map hiding (drop, map)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import qualified Ledger.Constraints as Constraints
import Perun.Onchain
import Plutus.Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (unless)
import Schema (ToSchema)
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
    spTimeLock :: !Integer
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
  deriving newtype (ToJSON, FromJSON)
  deriving (Generic)
  deriving stock (P.Eq, P.Show)

data DisputeParams = DisputeParams
  { dpSigningPKs :: ![PaymentPubKey],
    dpSignedState :: !SignedState
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

data CloseParams = CloseParams
  { cpSigningPKs :: ![PaymentPubKey],
    cpSignedState :: !SignedState
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

newtype ForceCloseParams = ForceCloseParams ChannelID
  deriving newtype (ToJSON, FromJSON)
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

--
-- open channel
--

start :: AsContractError e => OpenParams -> Contract w s e ()
start OpenParams {..} = do
  t <- currentTime
  let c =
        Channel
          { pTimeLock = spTimeLock,
            pSigningPKs = spSigningPKs,
            pPaymentPKs = spPaymentPKs
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
            funding = head spBalances : tail (map (const 0) spBalances),
            funded = False,
            disputed = False
          }
      v = Ada.lovelaceValueOf $ head spBalances
      tx = Constraints.mustPayToTheScript d v
  ledgerTx <- submitTxConstraints (typedChannelValidator spChannelId) tx
  void . awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $ printf "Started funding for channel %d with parameters %s and value %s" spChannelId (P.show c) (P.show v)

fund :: FundParams -> Contract w s Text ()
fund FundParams {..} = do
  (oref, o, d@ChannelDatum {..}) <- findChannel fpChannelId
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  -- TODO add more checks before funding
  when funded . throwError . pack $ printf "can only fund channel that is not already funded"
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
          P.<> Constraints.otherScript (channelValidator fpChannelId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        Constraints.mustPayToTheScript newDatum v
          <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void . awaitTxConfirmed $ getCardanoTxId ledgerTx
  -- TODO avoid using list-indexing with !!
  --      try to use fixed-sized arrays instead
  logInfo @P.String $ printf "Funded %d Lovelace for party %d on channel %d" (balances state !! fpIndex) fpIndex fpChannelId

abort :: AbortParams -> Contract w s Text ()
abort (AbortParams cId) = do
  (oref, o, d@ChannelDatum {..}) <- findChannel cId
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  when funded . throwError . pack $ printf "can not abort funded channel"
  let r = Redeemer (PlutusTx.toBuiltinData Abort)
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator cId)
          P.<> Constraints.otherScript (channelValidator cId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        mconcat (zipWith Constraints.mustPayToPubKey (pPaymentPKs channelParameters) (map Ada.lovelaceValueOf funding))
          <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void . awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $
    printf
      "aborted channel %d with parameters %s. The funding was: %s"
      cId
      (P.show channelParameters)
      (P.show funding)

-- sets the transaction values for forming the initial auction transaction (endpoint start)
open :: AsContractError e => OpenParams -> Contract w s e ()
open OpenParams {..} = do
  t <- currentTime
  let c =
        Channel
          { pTimeLock = spTimeLock,
            pSigningPKs = spSigningPKs,
            pPaymentPKs = spPaymentPKs
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
  logInfo @P.String $ printf "Opened channel %d with parameters %s and value %s" spChannelId (P.show c) (P.show v)

--
-- dispute logic
--

dispute :: DisputeParams -> Contract w s Text ()
dispute (DisputeParams keys sst) = do
  let dState = extractVerifiedState sst keys
  t <- currentTime
  (oref, o, d@ChannelDatum {..}) <- findChannel $ channelId dState
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  unless (isValidStateTransition state dState)
    . throwError
    . pack
    $ printf "state transition in dispute invalid"
  unless funded
    . throwError
    . pack
    $ printf "can only dispute on funded state"
  let newDatum =
        d
          { state = dState,
            time = t,
            disputed = True
          }
      v = Ada.lovelaceValueOf . sum $ balances state
      r = Redeemer . PlutusTx.toBuiltinData $ MkDispute sst

      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator $ channelId dState)
          P.<> Constraints.otherScript (channelValidator $ channelId dState)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        Constraints.mustPayToTheScript newDatum v
          <> Constraints.mustValidateIn (Ledger.intersection (from (t - defaultValidMsRangeSkew)) (to (t + defaultValidMsRange - defaultValidMsRangeSkew)))
          <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void . awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $ printf "made dispute of new state %s" (P.show dState)

--
-- close logic
--

close :: CloseParams -> Contract w s Text ()
close (CloseParams keys sst) = do
  let s@ChannelState {..} = extractVerifiedState sst keys
  (oref, o, d@ChannelDatum {..}) <- findChannel channelId
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  unless (isValidStateTransition state s) . throwError . pack $ printf "state transition invalid"
  unless final . throwError . pack $ printf "can not close unless state is final"
  unless funded . throwError . pack $ printf "can only close funded state"
  let r = Redeemer . PlutusTx.toBuiltinData $ MkClose sst
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator channelId)
          P.<> Constraints.otherScript (channelValidator channelId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        mconcat (zipWith Constraints.mustPayToPubKey (pPaymentPKs channelParameters) (map Ada.lovelaceValueOf balances))
          <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void . awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $
    printf
      "closed channel %d with params %s. The final balances are: %s"
      channelId
      (P.show channelParameters)
      (P.show balances)

--
-- close logic
--

forceClose :: ForceCloseParams -> Contract w s Text ()
forceClose (ForceCloseParams cId) = do
  (oref, o, d@ChannelDatum {..}) <- findChannel cId
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  unless disputed . throwError . pack $ printf "channel was never in disputed state"
  unless funded . throwError . pack $ printf "can only force-close funded state"
  let r = Redeemer (PlutusTx.toBuiltinData ForceClose)
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator cId)
          P.<> Constraints.otherScript (channelValidator cId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        mconcat (zipWith Constraints.mustPayToPubKey (pPaymentPKs channelParameters) (map Ada.lovelaceValueOf (balances state)))
          <> Constraints.mustValidateIn (from $ earliestFirstCloseTime time (pTimeLock channelParameters))
          <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void . awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $
    printf
      "force closed channel %d with parameters %s. The final balances are: %s"
      cId
      (P.show channelParameters)
      (P.show $ balances state)

-- | Calculates the earliest time a channel can be force-closed for given parameters.
earliestFirstCloseTime :: POSIXTime -> Integer -> POSIXTime
earliestFirstCloseTime timeStamp timeLock = timeStamp + POSIXTime timeLock

findChannel ::
  ChannelID ->
  Contract w s Text (TxOutRef, ChainIndexTxOut, ChannelDatum)
findChannel cID = do
  utxos <- utxosAt $ scriptHashAddress (channelHash cID)
  case Map.toList utxos of
    -- TODO: revise this!
    (oref, o) : _ -> case _ciTxOutDatum o of
      Left _ -> throwError "datum missing"
      Right (Datum e) -> case PlutusTx.fromBuiltinData e of
        Nothing -> throwError "datum has wrong type"
        Just d@ChannelDatum {} -> return (oref, o, d)
    _ -> throwError "channel utxo not found"

addFunding :: Integer -> Integer -> [Integer] -> [Integer]
addFunding amount index f =
  let (start, end) = genericSplitAt index f
   in start ++ amount : P.drop 1 end

--
-- Top-level contract, exposing all endpoints.
--
contract :: Contract () ChannelSchema Text ()
contract = selectList [start', fund', abort', open', dispute', close', forceClose'] >> contract
  where
    start' = endpoint @"start" start
    fund' = endpoint @"fund" fund
    abort' = endpoint @"abort" abort
    open' = endpoint @"open" open
    dispute' = endpoint @"dispute" dispute
    close' = endpoint @"close" close
    forceClose' = endpoint @"forceClose" forceClose
