{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Perun.Test.EvilContract where

import Control.Monad hiding (fmap)
import Control.Monad.Error.Lens (throwing)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data
import Data.Map as Map hiding (map)
import qualified Data.Semigroup as Semigroup
import GHC.Generics (Generic)
import Ledger
import qualified Ledger.Tx.Constraints as Constraints
import Perun hiding (abort, close, dispute, forceClose, fund, open, start)
import Perun.Error
import Plutus.Contract
import Plutus.Script.Utils.Ada as Ada
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified Plutus.V1.Ledger.Value as Value
import qualified PlutusTx
import PlutusTx.Prelude
import Text.Printf (printf)
import qualified Prelude as P

type EvilContractState = Maybe (Semigroup.Last TxId)

data FundCase = FundSteal | FundViolateChannelIntegrity | FundInvalidFunded | FundAlreadyFunded
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show, Data)

data EvilFund = EvilFund
  { efChannelId :: !ChannelID,
    efFunderIdx :: !Integer,
    efCase :: FundCase
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

data AbortCase = AbortInvalidFunding | AbortAlreadyFunded | AbortUnrelatedParticipant
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show, Data)

data EvilAbort = EvilAbort
  { eaChannelId :: !ChannelID,
    abCase :: AbortCase
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

data DisputeCase = DisputeInvalidInput | DisputeValidInput | DisputeWrongState
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show, Data)

data EvilDispute = EvilDispute
  { edChannelId :: !ChannelID,
    edSigningPKs :: ![PaymentPubKey],
    edPaymentPKs :: ![PaymentPubKeyHash],
    edBalances :: ![Integer],
    edSignedState :: !AllSignedStates,
    edCase :: !DisputeCase
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

data CloseCase = CloseInvalidInputValue | CloseUnfunded
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show, Data)

data EvilClose = EvilClose
  { ecChannelId :: ChannelID,
    ecSignedState :: !AllSignedStates,
    ecSigningPKs :: ![PaymentPubKey],
    ecBalances :: [Integer],
    ecCase :: CloseCase
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

data ForceCloseCase = ForceCloseInvalidInput | ForceCloseValidInput
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show, Data)

data EvilForceClose = EvilForceClose
  { efcChannelId :: !ChannelID,
    fcCase :: ForceCloseCase
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

type EvilSchema =
  Endpoint "fund" EvilFund
    .\/ Endpoint "abort" EvilAbort
    .\/ Endpoint "dispute" EvilDispute
    .\/ Endpoint "close" EvilClose
    .\/ Endpoint "forceClose" EvilForceClose

fund :: EvilFund -> Contract EvilContractState s PerunError ()
fund EvilFund {..} = do
  case efCase of
    FundSteal -> fundSteal efChannelId efFunderIdx
    FundViolateChannelIntegrity -> fundViolateChannelIntegrity efChannelId
    FundInvalidFunded -> fundInvalidFunded efChannelId
    FundAlreadyFunded -> fundAlreadyFunded efChannelId

fundViolateChannelIntegrity :: ChannelID -> Contract EvilContractState s PerunError ()
fundViolateChannelIntegrity cid = do
  (oref, o, d) <- findChannel cid
  logInfo @P.String $ printf "EVIL_CONTRACT: found channel utxo with datum %s" (P.show d)
  ct <- currentTime
  let _newFunding = []
      newDatum = d {time = ct}
      r = Scripts.Redeemer (PlutusTx.toBuiltinData Fund)
      v = Ada.toValue minAdaTxOutEstimated
  ledgerTx <- uncurry submitTxConstraintsWith $ uncheckedTx newDatum r cid v oref o
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx

-- | fundInvalidFunded sends a funding transaction which does not add any
-- additional funds to the channel but claims it is already funded.
fundInvalidFunded :: ChannelID -> Contract EvilContractState s PerunError ()
fundInvalidFunded cid = do
  (oref, o, d) <- findChannel cid
  logInfo @P.String $ printf "EVIL_CONTRACT: found channel utxo with datum %s" (P.show d)
  let newDatum = d {funded = True}
      r = Scripts.Redeemer (PlutusTx.toBuiltinData Fund)
      v = Ada.toValue minAdaTxOutEstimated
  ledgerTx <- uncurry submitTxConstraintsWith $ uncheckedTx newDatum r cid v oref o
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx

fundAlreadyFunded :: ChannelID -> Contract EvilContractState s PerunError ()
fundAlreadyFunded cid = do
  (oref, o, d@ChannelDatum {..}) <- findChannel cid
  if not funded
    then
      throwError . PerunContractError . OtherContractError $
        "EVIL_CONTRACT: endpoint only works for already funded channels"
    else do
      logInfo @P.String $ printf "EVIL_CONTRACT: found channel utxo with datum %s" (P.show d)
      let newDatum = d
          r = Scripts.Redeemer (PlutusTx.toBuiltinData Fund)
          v = Ada.toValue minAdaTxOutEstimated
      ledgerTx <- uncurry submitTxConstraintsWith $ uncheckedTx newDatum r cid v oref o
      void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
      tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx

fundSteal :: ChannelID -> Integer -> Contract EvilContractState s PerunError ()
fundSteal cid idx = do
  (oref, o, d@ChannelDatum {..}) <- findChannel cid
  logInfo @P.String $ printf "EVIL_CONTRACT: found channel utxo with datum %s" (P.show d)
  let (fs, _ : fs') = P.splitAt (P.fromIntegral idx P.- 1) $ P.replicate (P.length $ balances state) 0
      newFunding = fs ++ [sum funding] ++ fs'
      newDatum = d {funding = newFunding, funded = newFunding == balances state}
      v = Ada.lovelaceValueOf (sum newFunding)
      r = Scripts.Redeemer (PlutusTx.toBuiltinData Fund)
  ledgerTx <- uncurry submitTxConstraintsWith $ uncheckedTx newDatum r cid v oref o
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx
  logInfo @P.String "EVIL_CONTRACT: executed stealing of funds"

dropIndex :: Integer -> [a] -> Maybe [a]
dropIndex idx as | idx P.< 0 P.|| idx > length as P.|| P.null as = Nothing
dropIndex idx as = Just $ go idx 0 (as, [])
  where
    go i ci (a : as', bs)
      | i P.== ci = reverse bs P.++ as'
      | otherwise = go i (ci P.+ 1) (as', a : bs)
    go _ _ _ = P.error "impossible"

abort :: EvilAbort -> Contract EvilContractState s PerunError ()
abort (EvilAbort cid c) = case c of
  AbortInvalidFunding -> abortInvalidFunding cid
  AbortAlreadyFunded -> abortAlreadyFunded cid
  AbortUnrelatedParticipant -> abortUnrelatedParticipant cid

abortInvalidFunding :: ChannelID -> Contract EvilContractState s PerunError ()
abortInvalidFunding cid = do
  (oref, o, d) <- findChannel cid
  logInfo @P.String $ printf "EVIL_CONTRACT: found channel utxo with datum %s" (P.show d)
  let newDatum = d
      r = Scripts.Redeemer (PlutusTx.toBuiltinData Abort)
      v = Ada.toValue minAdaTxOutEstimated
  ledgerTx <- uncurry submitTxConstraintsWith $ uncheckedTx newDatum r cid v oref o
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx
  logInfo @P.String "EVIL_CONTRACT: executed malicious abort"

abortAlreadyFunded :: ChannelID -> Contract EvilContractState s PerunError ()
abortAlreadyFunded = abortInvalidFunding

abortUnrelatedParticipant :: ChannelID -> Contract EvilContractState s PerunError ()
abortUnrelatedParticipant = abortInvalidFunding

close :: EvilClose -> Contract EvilContractState s PerunError ()
close (EvilClose cid ss _pks bals c) = case c of
  CloseInvalidInputValue -> closeInvalidInputValue cid ss
  CloseUnfunded -> closeUnfunded cid ss bals

closeUnfunded :: ChannelID -> AllSignedStates -> [Integer] -> Contract EvilContractState s PerunError ()
closeUnfunded cid sst bals = do
  (oref, o, d@ChannelDatum {..}) <- findChannel cid
  logInfo @P.String $ printf "EVIL_CONTRACT: found channel utxo with datum %s" (P.show d)
  let newDatum = d
      r = Scripts.Redeemer . PlutusTx.toBuiltinData . MkClose $ compressSignatures sst
      v = Ada.toValue minAdaTxOutEstimated
      (lookups, tx) = uncheckedTx newDatum r cid v oref o
  ledgerTx <- submitTxConstraintsWith lookups (tx <> mconcat (zipWith Constraints.mustPayToPubKey (pPaymentPKs channelParameters) (map Ada.lovelaceValueOf bals)))
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx
  logInfo @P.String "EVIL_CONTRACT: executed malicious close"

closeInvalidInputValue :: ChannelID -> AllSignedStates -> Contract EvilContractState s PerunError ()
closeInvalidInputValue cid sst = do
  (oref, o, d) <- findChannel cid
  logInfo @P.String $ printf "EVIL_CONTRACT: found channel utxo with datum %s" (P.show d)
  let newDatum = d
      r = Scripts.Redeemer . PlutusTx.toBuiltinData . MkClose $ compressSignatures sst
      v = Ada.toValue minAdaTxOutEstimated
  ledgerTx <- uncurry submitTxConstraintsWith $ uncheckedTx newDatum r cid v oref o
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx
  logInfo @P.String "EVIL_CONTRACT: executed malicious close"

forceClose :: EvilForceClose -> Contract EvilContractState s PerunError ()
forceClose (EvilForceClose cid c) = case c of
  ForceCloseInvalidInput -> forceCloseInvalidInput cid
  ForceCloseValidInput -> forceCloseValidInput cid

forceCloseInvalidInput :: ChannelID -> Contract EvilContractState s PerunError ()
forceCloseInvalidInput cid = do
  (oref, o, d) <- findChannel cid
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  let newDatum = d
      r = Scripts.Redeemer . PlutusTx.toBuiltinData $ ForceClose
      v = Ada.toValue minAdaTxOutEstimated
  ledgerTx <- uncurry submitTxConstraintsWith $ uncheckedTx newDatum r cid v oref o
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx
  logInfo @P.String "EVIL_CONTRACT: executed malicious force close"

forceCloseValidInput :: ChannelID -> Contract EvilContractState s PerunError ()
forceCloseValidInput cid = do
  (oref, o, d@ChannelDatum {..}) <- findChannel cid
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  let r = Scripts.Redeemer (PlutusTx.toBuiltinData ForceClose)
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator cid)
          P.<> Constraints.plutusV2OtherScript (channelValidator cid)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        mconcat (zipWith Constraints.mustPayToPubKey (pPaymentPKs channelParameters) (map Ada.lovelaceValueOf (balances state)))
          <> Constraints.mustValidateIn (from $ earliestFirstCloseTime time (pTimeLock channelParameters))
          <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void . awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx
  logInfo @P.String "EVIL_CONTRACT: executed malicious force close"

dispute :: EvilDispute -> Contract EvilContractState s PerunError ()
dispute EvilDispute {..} = case edCase of
  DisputeInvalidInput -> disputeInvalidInput edChannelId edSignedState
  DisputeValidInput -> disputeValidInput edChannelId edSignedState edSigningPKs
  DisputeWrongState -> disputeWrongState edChannelId edSignedState

disputeWrongState :: ChannelID -> AllSignedStates -> Contract EvilContractState s PerunError ()
disputeWrongState cid sst = do
  (oref, o, d) <- findChannel cid
  disputeWithDatum oref o cid sst $ d {state = ChannelState (ChannelID "abc") [] 0 False}

disputeValidInput :: ChannelID -> AllSignedStates -> [PaymentPubKey] -> Contract EvilContractState s PerunError ()
disputeValidInput cid sst pubkeys = do
  let dState = extractVerifiedState (compressSignatures sst) pubkeys
  t <- currentTime
  (oref, o, d) <- findChannel cid
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  disputeWithDatum oref o cid sst d {state = dState, time = t, disputed = True}

disputeWithDatum :: TxOutRef -> DecoratedTxOut -> ChannelID -> AllSignedStates -> ChannelDatum -> Contract EvilContractState s PerunError ()
disputeWithDatum oref o cid sst dat = do
  let newDatum = dat
      r = Scripts.Redeemer . PlutusTx.toBuiltinData . MkDispute $ compressSignatures sst
      v = Ada.lovelaceValueOf . sum $ (balances . state $ dat)
  ledgerTx <- uncurry submitTxConstraintsWith $ uncheckedTx newDatum r cid v oref o
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx
  logInfo @P.String "EVIL_CONTRACT: executed malicious dispute"

disputeInvalidInput :: ChannelID -> AllSignedStates -> Contract EvilContractState s PerunError ()
disputeInvalidInput cid sst = do
  (oref, o, d) <- findChannel cid
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  let newDatum = d
      r = Scripts.Redeemer . PlutusTx.toBuiltinData . MkDispute $ compressSignatures sst
      v = Ada.toValue minAdaTxOutEstimated
  ledgerTx <- uncurry submitTxConstraintsWith $ uncheckedTx newDatum r cid v oref o
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx
  logInfo @P.String "EVIL_CONTRACT: executed malicious dispute"

--
-- Top-level contract, exposing all endpoints.
--
evilContract :: Contract EvilContractState EvilSchema PerunError ()
evilContract = selectList [fund', abort', dispute', close', forceClose'] >> evilContract
  where
    fund' = endpoint @"fund" fund
    abort' = endpoint @"abort" abort
    dispute' = endpoint @"dispute" dispute
    close' = endpoint @"close" close
    forceClose' = endpoint @"forceClose" forceClose

uncheckedTx :: PlutusTx.ToData o => o -> Scripts.Redeemer -> ChannelID -> Value.Value -> TxOutRef -> DecoratedTxOut -> (Constraints.ScriptLookups ChannelTypes, Constraints.TxConstraints ChannelAction o)
uncheckedTx d r cid v oref oidx = (lookups, tx)
  where
    lookups =
      Constraints.typedValidatorLookups (typedChannelValidator cid)
        P.<> Constraints.plutusV2OtherScript (channelValidator cid)
        P.<> Constraints.unspentOutputs (Map.singleton oref oidx)
    tx =
      Constraints.mustPayToTheScript d v
        <> Constraints.mustSpendScriptOutput oref r
