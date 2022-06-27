{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Types where
import PlutusTx.Prelude
import Ledger
import qualified Prelude as P
import GHC.Generics (Generic)
import qualified PlutusTx
import Data.Aeson
import Data.Data
import Plutus.Contract.Oracle
import qualified Ledger.Typed.Scripts as Scripts


type ChannelID = Integer

-- Parameters of the channel
data Channel = Channel
  { pTimeLock :: !Integer,
    pSigningPKA :: !PaymentPubKey,
    pSigningPKB :: !PaymentPubKey,
    pPaymentPKA :: !PaymentPubKeyHash,
    pPaymentPKB :: !PaymentPubKeyHash
  }
  deriving (P.Show, Generic, ToJSON, FromJSON)

-- Equality of two Channels
instance Eq Channel where
  {-# INLINEABLE (==) #-}
  a == b =
    (pTimeLock a == pTimeLock b)
      && (pSigningPKA a == pSigningPKA b)
      && (pSigningPKB a == pSigningPKB b)
      && (pPaymentPKA a == pPaymentPKA b)
      && (pPaymentPKB a == pPaymentPKB b)

PlutusTx.unstableMakeIsData ''Channel
PlutusTx.makeLift ''Channel

data ChannelState = ChannelState
  { channelId :: !ChannelID,
    balanceA :: !Integer,
    balanceB :: !Integer,
    version :: !Integer,
    final :: !Bool
  }
  deriving (Data)
  deriving stock (P.Eq, P.Show)

instance Eq ChannelState where
  {-# INLINEABLE (==) #-}
  b == c =
    (channelId b == channelId c)
      && (balanceA b == balanceA c)
      && (balanceB b == balanceB c)
      && (version b == version c)
      && (final b == final c)

PlutusTx.unstableMakeIsData ''ChannelState
PlutusTx.makeLift ''ChannelState

data SignedState = SignedState
  { sigA :: !(SignedMessage ChannelState),
    sigB :: !(SignedMessage ChannelState)
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

instance Eq SignedState where
  {-# INLINEABLE (==) #-}
  (SignedState a b) == (SignedState c d) =
    (a == c) && (b == d)



PlutusTx.unstableMakeIsData ''SignedState
PlutusTx.makeLift ''SignedState

-- Redeemer Datatype
data ChannelAction = MkDispute SignedState | MkClose SignedState | ForceClose
  deriving (P.Show)

PlutusTx.unstableMakeIsData ''ChannelAction
PlutusTx.makeLift ''ChannelAction

-- Datum datatype
data ChannelDatum = ChannelDatum
  { channelParameters :: !Channel,
    state :: !ChannelState,
    time :: !Ledger.POSIXTime,
    disputed :: !Bool
  }
  deriving (P.Show)

PlutusTx.unstableMakeIsData ''ChannelDatum
PlutusTx.makeLift ''ChannelDatum

-- Boilerplate code that allows us to use our custom types for Datum and
-- Redeemer in the Validator script instead of BuiltinData
data ChannelTypes

instance Scripts.ValidatorTypes ChannelTypes where
  type RedeemerType ChannelTypes = ChannelAction
  type DatumType ChannelTypes = ChannelDatum
