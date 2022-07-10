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

module PerunPlutus.Test.MockContract where

import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map as Map
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

type MockSchema =
  Endpoint "start" MockParams
    .\/ Endpoint "fund" MockParams
    .\/ Endpoint "abort" MockParams
    .\/ Endpoint "open" MockParams
    .\/ Endpoint "dispute" MockParams
    .\/ Endpoint "close" MockParams
    .\/ Endpoint "forceClose" MockParams

open :: AsContractError e => MockParams -> Contract w s e ()
open MockParams {..} = do
  let c =
        Channel
          { pTimeLock = mockTimeLock,
            pSigningPKs = mockSigningPKs,
            pPaymentPKs = mockPaymentPKs
          }
      s =
        ChannelState
          { channelId = mockChannelId,
            balances = mockBalances,
            version = mockVersion,
            final = mockFinal
          }
      d =
        ChannelDatum
          { channelParameters = c,
            state = s,
            time = mockTimeStamp,
            disputed = mockDisputed,
            funding = mockFunding,
            funded = mockFunded
          }
      v = Ada.lovelaceValueOf mockValue
      tx = Constraints.mustPayToTheScript d v
  ledgerTx <- submitTxConstraints (typedChannelValidator mockChannelId) tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $ printf "Opened channel %d with parameters %s and value %s" mockChannelId (P.show c) (P.show v)

start :: MockParams -> Contract w s Text ()
start MockParams {..} = PO.start $ OpenParams mockChannelId mockSigningPKs mockPaymentPKs mockBalances mockTimeLock

fund :: MockParams -> Contract w s Text ()
fund MockParams {..} = do
  (oref, o, d@ChannelDatum {..}) <- findChannel mockChannelId
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  let newFunding = mockFunding
      newDatum =
        d
          { funding = newFunding,
            funded = newFunding == balances state
          }
      v = Ada.lovelaceValueOf $ sum newFunding
      r = Redeemer $ PlutusTx.toBuiltinData Fund

      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator mockChannelId)
          P.<> Constraints.otherScript (channelValidator mockChannelId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        Constraints.mustPayToTheScript newDatum v
          <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

abort :: MockParams -> Contract w s Text ()
abort MockParams {..} = do
  (oref, o, d@ChannelDatum {..}) <- findChannel mockChannelId
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  let r = Redeemer $ PlutusTx.toBuiltinData Abort
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator mockChannelId)
          P.<> Constraints.otherScript (channelValidator mockChannelId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx = Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $
    printf
      "aborted channel %d with parameters %s. The funding was: %s"
      mockChannelId
      (P.show channelParameters)
      (P.show funding)

close :: MockParams -> Contract w s Text ()
close MockParams {..} = do
  sst <- case mockSignedState of
    Nothing -> throwError "missing signed state"
    Just s -> return s
  let ChannelState {..} = extractVerifiedState sst mockSigningPKs
  (oref, o, d@ChannelDatum {..}) <- findChannel channelId
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  let r = Redeemer $ PlutusTx.toBuiltinData $ MkClose sst
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

forceClose :: MockParams -> Contract w s Text ()
forceClose MockParams {..} = do
  (oref, o, d@ChannelDatum {..}) <- findChannel mockChannelId
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  let r = Redeemer $ PlutusTx.toBuiltinData ForceClose
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator mockChannelId)
          P.<> Constraints.otherScript (channelValidator mockChannelId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx = Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $
    printf
      "force closed channel %d with parameters %s. The final balances are: %s"
      mockChannelId
      (P.show channelParameters)
      (P.show $ balances state)

dispute :: MockParams -> Contract w s Text ()
dispute MockParams {..} = do
  (oref, o, d) <- findChannel mockChannelId
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  sst <- case mockSignedState of
    Nothing -> throwError "missing signed state"
    Just s -> return s
  let c =
        Channel
          { pTimeLock = mockTimeLock,
            pSigningPKs = mockSigningPKs,
            pPaymentPKs = mockPaymentPKs
          }
      s =
        ChannelState
          { channelId = mockChannelId,
            balances = mockBalances,
            version = mockVersion,
            final = mockFinal
          }
      datum =
        ChannelDatum
          { channelParameters = c,
            state = s,
            time = mockTimeStamp,
            disputed = mockDisputed,
            funding = mockFunding,
            funded = mockFunded
          }
      v = Ada.lovelaceValueOf mockValue

      r = Redeemer $ PlutusTx.toBuiltinData $ MkDispute sst
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator mockChannelId)
          P.<> Constraints.otherScript (channelValidator mockChannelId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        Constraints.mustPayToTheScript datum v
          <> Constraints.mustValidateIn (fromMaybe (always :: Interval POSIXTime) mockValidTimeRange)
          <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $ printf "made dispute of new state %s" (P.show s)

--
-- Top-level contract, exposing all endpoints.
--
mockContract :: Contract () MockSchema Text ()
mockContract = selectList [start', fund', abort', open', dispute', close', forceClose'] >> mockContract
  where
    start' = endpoint @"start" start
    fund' = endpoint @"fund" fund
    abort' = endpoint @"abort" abort
    open' = endpoint @"open" open
    dispute' = endpoint @"dispute" dispute
    close' = endpoint @"close" close
    forceClose' = endpoint @"forceClose" forceClose
