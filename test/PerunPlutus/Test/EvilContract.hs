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

module PerunPlutus.Test.EvilContract where

import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data
import Data.Map as Map hiding (map)
import qualified Data.Semigroup as Semigroup
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger
import Ledger.Ada as Ada
import qualified Ledger.Constraints as Constraints
import Perun hiding (abort, close, dispute, forceClose, fund, open, start)
import Plutus.Contract
import qualified PlutusTx
import PlutusTx.Prelude
import Text.Printf (printf)
import qualified Prelude as P

type EvilContractState = Maybe (Semigroup.Last TxId)

data FundCase = FundSteal | FundViolateChannelIntegrity | FundInvalidFunded | FundAlreadyFunded
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show, Data)

data EvilFund = EvilFund
  { efChannelId :: !Integer,
    efFunderIdx :: !Integer,
    efCase :: FundCase
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

data AbortCase = AbortInvalidFunding | AbortAlreadyFunded | AbortUnrelatedParticipant
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show, Data)

data EvilAbort = EvilAbort
  { eaChannelId :: Integer,
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
    edSignedState :: !SignedState,
    edCase :: !DisputeCase
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

data CloseCase = CloseInvalidInputValue | CloseUnfunded
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show, Data)

data EvilClose = EvilClose
  { ecChannelId :: ChannelID,
    ecSignedState :: !SignedState,
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
  { efcChannelId :: Integer,
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

fund :: EvilFund -> Contract EvilContractState s Text ()
fund EvilFund {..} = do
  case efCase of
    FundSteal -> fundSteal efChannelId efFunderIdx
    FundViolateChannelIntegrity -> fundViolateChannelIntegrity efChannelId
    FundInvalidFunded -> fundInvalidFunded efChannelId
    FundAlreadyFunded -> fundAlreadyFunded efChannelId

fundViolateChannelIntegrity :: ChannelID -> Contract EvilContractState s Text ()
fundViolateChannelIntegrity cid = do
  (oref, o, d) <- findChannel cid
  logInfo @P.String $ printf "EVIL_CONTRACT: found channel utxo with datum %s" (P.show d)
  ct <- currentTime
  let _newFunding = []
      newDatum = d {time = ct}
      r = Redeemer (PlutusTx.toBuiltinData Fund)
      v = Ada.toValue minAdaTxOut
  ledgerTx <- uncurry submitTxConstraintsWith $ uncheckedTx newDatum r cid v oref o
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx

-- | fundInvalidFunded sends a funding transaction which does not add any
-- additional funds to the channel but claims it is already funded.
fundInvalidFunded :: ChannelID -> Contract EvilContractState s Text ()
fundInvalidFunded cid = do
  (oref, o, d) <- findChannel cid
  logInfo @P.String $ printf "EVIL_CONTRACT: found channel utxo with datum %s" (P.show d)
  let newDatum = d {funded = True}
      r = Redeemer (PlutusTx.toBuiltinData Fund)
      v = Ada.toValue minAdaTxOut
  ledgerTx <- uncurry submitTxConstraintsWith $ uncheckedTx newDatum r cid v oref o
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx

fundAlreadyFunded :: ChannelID -> Contract EvilContractState s Text ()
fundAlreadyFunded cid = do
  (oref, o, d@ChannelDatum {..}) <- findChannel cid
  if not funded
    then throwError "EVIL_CONTRACT: endpoint only works for already funded channels"
    else do
      logInfo @P.String $ printf "EVIL_CONTRACT: found channel utxo with datum %s" (P.show d)
      let newDatum = d
          r = Redeemer (PlutusTx.toBuiltinData Fund)
          v = Ada.toValue minAdaTxOut
      ledgerTx <- uncurry submitTxConstraintsWith $ uncheckedTx newDatum r cid v oref o
      void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
      tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx

fundSteal :: ChannelID -> Integer -> Contract EvilContractState s Text ()
fundSteal cid idx = do
  (oref, o, d@ChannelDatum {..}) <- findChannel cid
  logInfo @P.String $ printf "EVIL_CONTRACT: found channel utxo with datum %s" (P.show d)
  let (fs, _ : fs') = P.splitAt (P.fromIntegral idx P.- 1) $ P.replicate (P.length $ balances state) 0
      newFunding = fs ++ [sum funding] ++ fs'
      newDatum = d {funding = newFunding, funded = newFunding == balances state}
      v = Ada.lovelaceValueOf (sum newFunding)
      r = Redeemer (PlutusTx.toBuiltinData Fund)
  ledgerTx <- uncurry submitTxConstraintsWith $ uncheckedTx newDatum r cid v oref o
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx
  logInfo @P.String "EVIL_CONTRACT: executed stealing of funds"

dropIndex :: Integer -> [a] -> Maybe [a]
dropIndex idx as | idx P.< 0 P.|| idx > length as P.|| P.null as = Nothing
dropIndex idx as = Just $ go idx 0 (as, [])
  where
    go i ci (a : as, bs)
      | i P.== ci = reverse bs P.++ as
      | otherwise = go i (ci P.+ 1) (as, a : bs)
    go _ _ _ = P.error "impossible"

abort :: EvilAbort -> Contract EvilContractState s Text ()
abort (EvilAbort cid c) = case c of
  AbortInvalidFunding -> abortInvalidFunding cid
  AbortAlreadyFunded -> abortAlreadyFunded cid
  AbortUnrelatedParticipant -> abortUnrelatedParticipant cid

abortInvalidFunding :: ChannelID -> Contract EvilContractState s Text ()
abortInvalidFunding cid = do
  (oref, o, d) <- findChannel cid
  logInfo @P.String $ printf "EVIL_CONTRACT: found channel utxo with datum %s" (P.show d)
  let newDatum = d
      r = Redeemer (PlutusTx.toBuiltinData Abort)
      v = Ada.toValue minAdaTxOut
  ledgerTx <- uncurry submitTxConstraintsWith $ uncheckedTx newDatum r cid v oref o
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx
  logInfo @P.String "EVIL_CONTRACT: executed malicious abort"

abortAlreadyFunded :: ChannelID -> Contract EvilContractState s Text ()
abortAlreadyFunded = abortInvalidFunding

abortUnrelatedParticipant :: ChannelID -> Contract EvilContractState s Text ()
abortUnrelatedParticipant = abortInvalidFunding

close :: EvilClose -> Contract EvilContractState s Text ()
close (EvilClose cid ss _pks bals c) = case c of
  CloseInvalidInputValue -> closeInvalidInputValue cid ss
  CloseUnfunded -> closeUnfunded cid ss bals

closeUnfunded :: ChannelID -> SignedState -> [Integer] -> Contract EvilContractState s Text ()
closeUnfunded cid sst bals = do
  (oref, o, d@ChannelDatum {..}) <- findChannel cid
  logInfo @P.String $ printf "EVIL_CONTRACT: found channel utxo with datum %s" (P.show d)
  let newDatum = d
      r = Redeemer . PlutusTx.toBuiltinData . MkClose $ sst
      v = Ada.toValue minAdaTxOut
      (lookup, tx) = uncheckedTx newDatum r cid v oref o
  ledgerTx <- submitTxConstraintsWith lookup (tx <> mconcat (zipWith Constraints.mustPayToPubKey (pPaymentPKs channelParameters) (map Ada.lovelaceValueOf bals)))
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx
  logInfo @P.String "EVIL_CONTRACT: executed malicious close"

closeInvalidInputValue :: ChannelID -> SignedState -> Contract EvilContractState s Text ()
closeInvalidInputValue cid sst = do
  (oref, o, d) <- findChannel cid
  logInfo @P.String $ printf "EVIL_CONTRACT: found channel utxo with datum %s" (P.show d)
  let newDatum = d
      r = Redeemer . PlutusTx.toBuiltinData . MkClose $ sst
      v = Ada.toValue minAdaTxOut
  ledgerTx <- uncurry submitTxConstraintsWith $ uncheckedTx newDatum r cid v oref o
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx
  logInfo @P.String "EVIL_CONTRACT: executed malicious close"

forceClose :: EvilForceClose -> Contract EvilContractState s Text ()
forceClose (EvilForceClose cid c) = case c of
  ForceCloseInvalidInput -> forceCloseInvalidInput cid
  ForceCloseValidInput -> forceCloseValidInput cid

forceCloseInvalidInput :: ChannelID -> Contract EvilContractState s Text ()
forceCloseInvalidInput cid = do
  (oref, o, d) <- findChannel cid
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  let newDatum = d
      r = Redeemer . PlutusTx.toBuiltinData $ ForceClose
      v = Ada.toValue minAdaTxOut
  ledgerTx <- uncurry submitTxConstraintsWith $ uncheckedTx newDatum r cid v oref o
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx
  logInfo @P.String "EVIL_CONTRACT: executed malicious force close"

forceCloseValidInput :: ChannelID -> Contract EvilContractState s Text ()
forceCloseValidInput cid = do
  (oref, o, d@ChannelDatum {..}) <- findChannel cid
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  let r = Redeemer (PlutusTx.toBuiltinData ForceClose)
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator cid)
          P.<> Constraints.otherScript (channelValidator cid)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        mconcat (zipWith Constraints.mustPayToPubKey (pPaymentPKs channelParameters) (map Ada.lovelaceValueOf (balances state)))
          <> Constraints.mustValidateIn (from $ earliestFirstCloseTime time (pTimeLock channelParameters))
          <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void . awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx
  logInfo @P.String "EVIL_CONTRACT: executed malicious force close"

dispute :: EvilDispute -> Contract EvilContractState s Text ()
dispute EvilDispute {..} = case edCase of
  DisputeInvalidInput -> disputeInvalidInput edChannelId edSignedState
  DisputeValidInput -> disputeValidInput edChannelId edSignedState edSigningPKs
  DisputeWrongState -> disputeWrongState edChannelId edSignedState

disputeWrongState :: ChannelID -> SignedState -> Contract EvilContractState s Text ()
disputeWrongState cid sst = do
  (oref, o, d) <- findChannel cid
  disputeWithDatum oref o cid sst $ d {state = ChannelState 99 [] 0 False}

disputeValidInput :: ChannelID -> SignedState -> [PaymentPubKey] -> Contract EvilContractState s Text ()
disputeValidInput cid sst keys = do
  let dState = extractVerifiedState sst keys
  t <- currentTime
  (oref, o, d) <- findChannel cid
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  disputeWithDatum oref o cid sst d {state = dState, time = t, disputed = True}

disputeWithDatum :: TxOutRef -> ChainIndexTxOut -> ChannelID -> SignedState -> ChannelDatum -> Contract EvilContractState s Text ()
disputeWithDatum oref o cid sst dat = do
  let newDatum = dat
      r = Redeemer . PlutusTx.toBuiltinData . MkDispute $ sst
      v = Ada.lovelaceValueOf . sum $ (balances . state $ dat)
  ledgerTx <- uncurry submitTxConstraintsWith $ uncheckedTx newDatum r cid v oref o
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx
  logInfo @P.String "EVIL_CONTRACT: executed malicious dispute"

disputeInvalidInput :: ChannelID -> SignedState -> Contract EvilContractState s Text ()
disputeInvalidInput cid sst = do
  (oref, o, d) <- findChannel cid
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  let newDatum = d
      r = Redeemer . PlutusTx.toBuiltinData . MkDispute $ sst
      v = Ada.toValue minAdaTxOut
  ledgerTx <- uncurry submitTxConstraintsWith $ uncheckedTx newDatum r cid v oref o
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  tell . Just . Semigroup.Last . getCardanoTxId $ ledgerTx
  logInfo @P.String "EVIL_CONTRACT: executed malicious dispute"

--
-- Top-level contract, exposing all endpoints.
--
evilContract :: Contract EvilContractState EvilSchema Text ()
evilContract = selectList [fund', abort', dispute', close', forceClose'] >> evilContract
  where
    fund' = endpoint @"fund" fund
    abort' = endpoint @"abort" abort
    dispute' = endpoint @"dispute" dispute
    close' = endpoint @"close" close
    forceClose' = endpoint @"forceClose" forceClose

uncheckedTx :: PlutusTx.ToData o => o -> Redeemer -> ChannelID -> Value -> TxOutRef -> ChainIndexTxOut -> (Constraints.ScriptLookups ChannelTypes, Constraints.TxConstraints ChannelAction o)
uncheckedTx d r cid v oref oidx = (lookups, tx)
  where
    lookups =
      Constraints.typedValidatorLookups (typedChannelValidator cid)
        P.<> Constraints.otherScript (channelValidator cid)
        P.<> Constraints.unspentOutputs (Map.singleton oref oidx)
    tx =
      Constraints.mustPayToTheScript d v
        <> Constraints.mustSpendScriptOutput oref r
