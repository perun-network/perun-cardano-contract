{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
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
import Data.Map as Map hiding (map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger
import Ledger.Ada as Ada
import qualified Ledger.Constraints as Constraints
import Perun hiding (abort, close, dispute, forceClose, fund, open, start)
import qualified Perun.Offchain as PO
import Plutus.Contract
import qualified PlutusTx
import PlutusTx.Prelude
import Text.Printf (printf)
import qualified Prelude as P

data MockParams = MockParams
  { mockChannelId :: !ChannelID,
    mockSigningPKs :: ![PaymentPubKey],
    mockPaymentPKs :: ![PaymentPubKeyHash],
    mockBalances :: ![Integer],
    mockValue :: !Integer,
    mockVersion :: !Integer,
    mockFinal :: !Bool,
    mockTimeLock :: !Integer,
    mockTimeStamp :: !POSIXTime,
    mockDisputed :: !Bool,
    mockSignedState :: !(Maybe SignedState),
    mockValidTimeRange :: !(Maybe POSIXTimeRange),
    mockFunded :: !Bool,
    mockFunding :: ![Integer]
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

data EvilOpen = EvilOpen
  { eoChannelId :: !ChannelID,
    eoTimeLock :: !Integer,
    eoSigningPKs :: ![PaymentPubKey],
    eoBalances :: ![Integer],
    eoVersion :: !Integer,
    eoValue :: !Integer,
    eoTimeStamp :: !POSIXTime,
    eoPaymentPKs :: ![PaymentPubKeyHash],
    eoDisputed :: !Bool,
    eoFinal :: !Bool,
    eoFunded :: !Bool,
    eoFunding :: ![Integer]
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

type EvilStart = OpenParams

data EvilFund = EvilFund
  { efChannelId :: !Integer,
    efFunding :: ![Integer]
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

newtype EvilAbort = EvilAbort
  { eaChannelId :: Integer
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

data EvilDispute = EvilDispute
  { edChannelId :: !ChannelID,
    edTimeLock :: !Integer,
    edSigningPKs :: ![PaymentPubKey],
    edBalances :: ![Integer],
    edVersion :: !Integer,
    edValue :: !Integer,
    edSignedState :: !SignedState,
    edTimeStamp :: !POSIXTime,
    edPaymentPKs :: ![PaymentPubKeyHash],
    edDisputed :: !Bool,
    edValidTimeRange :: !(Interval POSIXTime),
    edFinal :: !Bool,
    edFunded :: !Bool,
    edFunding :: ![Integer]
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

data EvilClose = EvilClose
  { ecSignedState :: !SignedState,
    ecSigningPKs :: ![PaymentPubKey]
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

newtype EvilForceClose = EvilForceClose
  { efcChannelId :: Integer
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

type EvilSchema =
  Endpoint "start" EvilStart
    .\/ Endpoint "fund" EvilFund
    .\/ Endpoint "abort" EvilAbort
    .\/ Endpoint "open" EvilOpen
    .\/ Endpoint "dispute" EvilDispute
    .\/ Endpoint "close" EvilClose
    .\/ Endpoint "forceClose" EvilForceClose

open :: EvilOpen -> Contract w s Text ()
open EvilOpen {..} = do
  let c =
        Channel
          { pTimeLock = eoTimeLock,
            pSigningPKs = eoSigningPKs,
            pPaymentPKs = eoPaymentPKs
          }
      s =
        ChannelState
          { channelId = eoChannelId,
            balances = eoBalances,
            version = eoVersion,
            final = eoFinal
          }
      d =
        ChannelDatum
          { channelParameters = c,
            state = s,
            time = eoTimeStamp,
            disputed = eoDisputed,
            funding = eoFunding,
            funded = eoFunded
          }
      v = Ada.lovelaceValueOf eoValue
      tx = Constraints.mustPayToTheScript d v
  ledgerTx <- submitTxConstraints (typedChannelValidator eoChannelId) tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $ printf "Opened channel %d with parameters %s and value %s" eoChannelId (P.show c) (P.show v)

start :: EvilStart -> Contract w s Text ()
start = PO.start

fund :: EvilFund -> Contract w s Text ()
fund EvilFund {..} = do
  (oref, o, d@ChannelDatum {..}) <- findChannel efChannelId
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  let newFunding = efFunding
      newDatum =
        d
          { funding = newFunding,
            funded = newFunding == balances state
          }
      v = Ada.lovelaceValueOf $ sum newFunding
      r = Redeemer $ PlutusTx.toBuiltinData Fund

      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator efChannelId)
          P.<> Constraints.otherScript (channelValidator efChannelId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        Constraints.mustPayToTheScript newDatum v
          <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

abort :: EvilAbort -> Contract w s Text ()
abort EvilAbort {..} = do
  (oref, o, d@ChannelDatum {..}) <- findChannel eaChannelId
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  let r = Redeemer $ PlutusTx.toBuiltinData Abort
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator eaChannelId)
          P.<> Constraints.otherScript (channelValidator eaChannelId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx = Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $
    printf
      "aborted channel %d with parameters %s. The funding was: %s"
      eaChannelId
      (P.show channelParameters)
      (P.show funding)

close :: EvilClose -> Contract w s Text ()
close EvilClose {..} = do
  let ChannelState {..} = extractVerifiedState ecSignedState ecSigningPKs
  (oref, o, d@ChannelDatum {..}) <- findChannel channelId
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  let r = Redeemer $ PlutusTx.toBuiltinData $ MkClose ecSignedState
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator channelId)
          P.<> Constraints.otherScript (channelValidator channelId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx = Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $
    printf
      "closed channel %d with params %s. The final balances are: %s"
      channelId
      (P.show channelParameters)
      (P.show balances)

forceClose :: EvilForceClose -> Contract w s Text ()
forceClose EvilForceClose {..} = do
  (oref, o, d@ChannelDatum {..}) <- findChannel efcChannelId
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  let r = Redeemer $ PlutusTx.toBuiltinData ForceClose
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator efcChannelId)
          P.<> Constraints.otherScript (channelValidator efcChannelId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx = Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $
    printf
      "force closed channel %d with parameters %s. The final balances are: %s"
      efcChannelId
      (P.show channelParameters)
      (P.show $ balances state)

dispute :: EvilDispute -> Contract w s Text ()
dispute EvilDispute {..} = do
  (oref, o, d) <- findChannel edChannelId
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  let c =
        Channel
          { pTimeLock = edTimeLock,
            pSigningPKs = edSigningPKs,
            pPaymentPKs = edPaymentPKs
          }
      s =
        ChannelState
          { channelId = edChannelId,
            balances = edBalances,
            version = edVersion,
            final = edFinal
          }
      datum =
        ChannelDatum
          { channelParameters = c,
            state = s,
            time = edTimeStamp,
            disputed = edDisputed,
            funding = edFunding,
            funded = edFunded
          }
      v = Ada.lovelaceValueOf edValue

      r = Redeemer $ PlutusTx.toBuiltinData $ MkDispute edSignedState
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator edChannelId)
          P.<> Constraints.otherScript (channelValidator edChannelId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        Constraints.mustPayToTheScript datum v
          <> Constraints.mustValidateIn edValidTimeRange
          <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $ printf "made dispute of new state %s" (P.show s)

--
-- Top-level contract, exposing all endpoints.
--
evilContract :: Contract () EvilSchema Text ()
evilContract = selectList [start', fund', abort', open', dispute', close', forceClose'] >> evilContract
  where
    start' = endpoint @"start" start
    fund' = endpoint @"fund" fund
    abort' = endpoint @"abort" abort
    open' = endpoint @"open" open
    dispute' = endpoint @"dispute" dispute
    close' = endpoint @"close" close
    forceClose' = endpoint @"forceClose" forceClose
