{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}


module Verifier.Honest where

import Verifier.Verifier
import Plutus.Contract (currentTime, Contract)
import Types
import Ledger
import GHC.Generics (Generic)
import Data.Aeson
import qualified Prelude as P
import PlutusTx.Prelude
import qualified Plutus.V1.Ledger.Ada as Ada
import qualified Ledger.Constraints as Constraints
import qualified PlutusTx
import Data.Map as Map

data OpenParams = OpenParams
  { spChannelId :: !ChannelID,
    spSigningPKA :: !PaymentPubKey,
    spSigningPKB :: !PaymentPubKey,
    spPaymentPKA :: !PaymentPubKeyHash,
    spPaymentPKB :: !PaymentPubKeyHash,
    spBalanceA :: !Integer,
    spBalanceB :: !Integer,
    spTimeLock :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

data DisputeParams = DisputeParams
  { dpSigningPKA :: !PaymentPubKey,
    dpSigningPKB :: !PaymentPubKey,
    dpSignedState :: !SignedState
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

data CloseParams = CloseParams
  { cpSigningPKA :: !PaymentPubKey,
    cpSigningPKB :: !PaymentPubKey,
    cpSignedState :: !SignedState
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

newtype ForceCloseParams = ForceCloseParams ChannelID
  deriving newtype (ToJSON, FromJSON)
  deriving (Generic)
  deriving stock (P.Eq, P.Show)


type instance VerifierParams OpenType = OpenParams
type instance VerifierParams DisputeType = DisputeParams
type instance VerifierParams CloseType = CloseParams
type instance VerifierParams ForceCloseType = ForceCloseParams



type HonestVerifier w s e p a = Contract w s e a

instance Verifier (HonestVerifier w s e) where
  verifyOpen OpenParams {..} = do
    t <- currentTime
    let c =
          Channel
          { pTimeLock = spTimeLock,
            pSigningPKA = spSigningPKA,
            pSigningPKB = spSigningPKB,
            pPaymentPKA = spPaymentPKA,
            pPaymentPKB = spPaymentPKB
          }
        s =
          ChannelState
          { channelId = spChannelId,
            balanceA = spBalanceA,
            balanceB = spBalanceB,
            version = 0,
            final = False
          }
        d =
          ChannelDatum
          { channelParameters = c,
            state = s,
            time = t,
            disputed = False
          }
        v = Ada.lovelaceValueOf (spBalanceA + spBalanceB)
        tx = Constraints.mustPayToTheScript d v
    return (typedChannelValidator spChannelId, tx)

  verifyDispute (DisputeParams pKA pKB sst) = do
    let dState = extractVerifiedState sst (pKA, pKB)
    t <- currentTime
    (oref, o, d@ChannelDatum {..}) <- findChannel $ channelId dState
    logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
    unless (isValidStateTransition state dState) $
      throwError $ pack $ printf "state transition in dispute invalid"
    let
        newDatum =
          d
            { state = dState,
              time = t,
              disputed = True
            }
        v = Ada.lovelaceValueOf (balanceA state + balanceB state)
        r = Redeemer $ PlutusTx.toBuiltinData $ MkDispute sst

        lookups =
          Constraints.typedValidatorLookups (typedChannelValidator $ channelId dState)
            P.<> Constraints.otherScript (channelValidator $ channelId dState)
            P.<> Constraints.unspentOutputs (Map.singleton oref o)
        tx =
          Constraints.mustPayToTheScript newDatum v
            <> Constraints.mustValidateIn (Ledger.intersection (from (t - 1000)) (to (t + defaultValidMsRange - 1000)))
            <> Constraints.mustSpendScriptOutput oref r
    return (lookups, tx)

  verifyClose (CloseParams pKA pKB sst) = do
    let s@ChannelState{..} = extractVerifiedState sst (pKA, pKB)
    (oref, o, d@ChannelDatum {..}) <- findChannel channelId
    logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
    unless (isValidStateTransition state s) $
      throwError $ pack $ printf "state transition invalid"
    unless final $
      throwError $ pack $ printf "can not close unless state is final"
    let
      r = Redeemer $ PlutusTx.toBuiltinData $ MkClose sst
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator channelId)
          P.<> Constraints.otherScript (channelValidator channelId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        Constraints.mustPayToPubKey (pPaymentPKA channelParameters) (Ada.lovelaceValueOf balanceA)
          <> Constraints.mustPayToPubKey (pPaymentPKB channelParameters) (Ada.lovelaceValueOf balanceB)
          <> Constraints.mustSpendScriptOutput oref r
    return (lookups, tx)

  verifyForceClose (ForceCloseParams cId) = do
    (oref, o, d@ChannelDatum {..}) <- findChannel cId
    logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
    unless disputed $
      throwError $ pack $ printf "channel was never in disputed state"
    let r = Redeemer $ PlutusTx.toBuiltinData ForceClose
        lookups =
          Constraints.typedValidatorLookups (typedChannelValidator cId)
            P.<> Constraints.otherScript (channelValidator cId)
            P.<> Constraints.unspentOutputs (Map.singleton oref o)
        tx =
          Constraints.mustPayToPubKey (pPaymentPKA channelParameters) (Ada.lovelaceValueOf (balanceA state))
            <> Constraints.mustPayToPubKey (pPaymentPKB channelParameters) (Ada.lovelaceValueOf (balanceB state))
            <> Constraints.mustValidateIn (from (time + 1 + fromMilliSeconds (DiffMilliSeconds (pTimeLock channelParameters))))
            <> Constraints.mustSpendScriptOutput oref r
    return (lookups, tx)