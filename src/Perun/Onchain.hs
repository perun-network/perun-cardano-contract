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

module Perun.Onchain
  ( Channel (..),
    ChannelState (..),
    SignedState (..),
    ChannelAction (..),
    ChannelDatum (..),
    ChannelTypes,
    ChannelID,
    ensureKnownCurrencies,
    isValidStateTransition,
    extractVerifiedState,
    defaultValidMsRange,
    defaultValidMsRangeSkew,
    printJson,
    printSchemas,
    stage,
    typedChannelValidator,
    channelValidator,
    channelHash,
    channelAddress,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract (ensureKnownCurrencies, printJson, printSchemas, stage)
import Plutus.Contract.Oracle (SignedMessage, verifySignedMessageConstraints)
import qualified PlutusTx
import PlutusTx.Prelude hiding (unless)
import qualified Prelude as P

--
--
-- ON CHAIN PART
--
--

type ChannelID = Integer

-- Parameters of the channel
data Channel = Channel
  { pTimeLock :: !Integer,
    pSigningPKs :: ![PaymentPubKey],
    pPaymentPKs :: ![PaymentPubKeyHash]
  }
  deriving (P.Show, Generic, ToJSON, FromJSON)

-- Equality of two Channels
instance Eq Channel where
  {-# INLINEABLE (==) #-}
  a == b =
    pTimeLock a == pTimeLock b
      && pSigningPKs a == pSigningPKs b
      && pPaymentPKs a == pPaymentPKs b

PlutusTx.unstableMakeIsData ''Channel
PlutusTx.makeLift ''Channel

data ChannelState = ChannelState
  { channelId :: !ChannelID,
    balances :: ![Integer],
    version :: !Integer,
    final :: !Bool
  }
  deriving (Data, Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

instance Eq ChannelState where
  {-# INLINEABLE (==) #-}
  b == c =
    channelId b == channelId c
      && balances b == balances c
      && version b == version c
      && final b == final c

PlutusTx.unstableMakeIsData ''ChannelState
PlutusTx.makeLift ''ChannelState

newtype SignedState = SignedState
  { stateSigs :: [SignedMessage ChannelState]
  }
  deriving (Generic)
  deriving newtype (ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

instance Eq SignedState where
  {-# INLINEABLE (==) #-}
  a == b = stateSigs a == stateSigs b

PlutusTx.unstableMakeIsData ''SignedState
PlutusTx.makeLift ''SignedState

-- Redeemer Datatype
data ChannelAction = Fund | Abort | MkDispute SignedState | MkClose SignedState | ForceClose
  deriving (P.Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''ChannelAction
PlutusTx.makeLift ''ChannelAction

-- Datum datatype
data ChannelDatum = ChannelDatum
  { channelParameters :: !Channel,
    state :: !ChannelState,
    time :: !Ledger.POSIXTime,
    funding :: ![Integer],
    funded :: !Bool,
    disputed :: !Bool
  }
  deriving (P.Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''ChannelDatum
PlutusTx.makeLift ''ChannelDatum

-- Boilerplate code that allows us to use our custom types for Datum and
-- Redeemer in the Validator script instead of BuiltinData
data ChannelTypes

instance Scripts.ValidatorTypes ChannelTypes where
  type RedeemerType ChannelTypes = ChannelAction
  type DatumType ChannelTypes = ChannelDatum

defaultValidMsRange :: POSIXTime
defaultValidMsRange = 10000

defaultValidMsRangeSkew :: POSIXTime
defaultValidMsRangeSkew = 1000

-- | Returns true, iff the new state is a valid post-state of the old channel state.
--  A valid state transition must retain the channelId and the sum of the balances.
--  The new version number must be greater than the old version number and there is
--  no valid transition from a final state.
{-# INLINEABLE isValidStateTransition #-}
isValidStateTransition :: ChannelState -> ChannelState -> Bool
isValidStateTransition old new =
  channelId old == channelId new
    && sum (balances old) == sum (balances new)
    && version old < version new
    && not (final old)

{-# INLINEABLE extractVerifiedState #-}

-- | Given a signed state and a list of public keys this returns the channel state `s`,
-- iff the signed state consists of exactly one valid signature on `s` for every given
-- public key in the correct order. It throws an error otherwise.
extractVerifiedState :: SignedState -> [PaymentPubKey] -> ChannelState
extractVerifiedState (SignedState sigs) signingKeys =
  let states =
        zipWithEqualLength
          verifySignedMessageConstraints'
          signingKeys
          sigs
          "list of signature and list of keys have different lengths"
   in if and $ PlutusTx.Prelude.map (== head states) (tail states)
        then
          ( case head states of
              Just s -> s
              Nothing -> traceError "invalid signatures"
          )
        else traceError "invalidSignatures"

-- | This performs `zipWith` on the first three arguments, iff both lists are of equal length
-- and throws an error with the given message otherwise.
{-# INLINEABLE zipWithEqualLength #-}
zipWithEqualLength :: forall a b c. (a -> b -> c) -> [a] -> [b] -> BuiltinString -> [c]
zipWithEqualLength f lstA lstB msg =
  if length lstA == length lstB
    then zipWith f lstA lstB
    else traceError msg

{-# INLINEABLE verifySignedMessageConstraints' #-}
verifySignedMessageConstraints' :: PaymentPubKey -> SignedMessage ChannelState -> Maybe ChannelState
verifySignedMessageConstraints' sKey sm = case verifySignedMessageConstraints sKey sm of
  Left _ -> Nothing
  Right (s, _ :: Constraints.TxConstraints () ()) -> Just s

-- Params:
--  Datum
--  Redeemer
--  Consuming Transaction
-- Returns:
--  Bool: Transaction is valid, iff this returns true and no errors are thrown
{-# INLINEABLE mkChannelValidator #-}
mkChannelValidator :: ChannelID -> ChannelDatum -> ChannelAction -> ScriptContext -> Bool
mkChannelValidator cID oldDatum action ctx =
  case action of
    Fund ->
      and
        [ -- can not fund already funded channels
          traceIfFalse "channel is already funded" (not $ funded oldDatum),
          -- check that the new funding state sums up to the value of the script inputs
          traceIfFalse "value of script output does not reflect funding" (correctInputFunding && correctChannelFunding),
          -- check that every funding value increases monotonously
          traceIfFalse "invalid funding" (all (== True) (zipWith (<=) (funding oldDatum) (funding outputDatum))),
          -- check that parameters, state, time stay the same, disputed == false
          traceIfFalse "violated channel integrity" channelIntegrityAtFunding,
          -- check that the channel is marked as funded iff it is actually funded
          -- the channel is funded iff the funding reflects the state in the output datum
          traceIfFalse "funded flag incorrect in output datum" checkFundingStatus
        ]
    Abort ->
      traceIfFalse "wrong input funding" correctInputFunding
        &&
        -- no aborts on funded channels
        traceIfFalse "channel is already funded" (not $ funded oldDatum)
        &&
        -- check the authenticity of the abort to prevent DOS
        traceIfFalse "abort must be issued by channel participant" (any (txSignedBy info . unPaymentPubKeyHash) (pPaymentPKs $ channelParameters oldDatum))
        &&
        -- check that every party gets their funding refunded
        traceIfFalse "A party was not reimbursed correctly for their funding" (all (== True) (zipWith getsValue (pPaymentPKs (channelParameters oldDatum)) (funding oldDatum)))
    -- Dispute Case:
    MkDispute st ->
      let newState = extractVerifiedState st (pSigningPKs $ channelParameters oldDatum)
       in and
            [ traceIfFalse "wrong input value" correctInputValue,
              traceIfFalse "old state must be funded" (funded oldDatum),
              traceIfFalse "new state must be funded" (funded outputDatum),
              -- check that the state in the dispute is reflected in the output datum
              traceIfFalse "output state does not match the state in the dispute" (newState == state outputDatum),
              -- check that the state transition is valid
              traceIfFalse "invalid state transition" (isValidStateTransition oldState (state outputDatum)),
              -- check that the channel id in the state in the dispute matches the actual channel id
              traceIfFalse "state in dispute does not belong to this channel" (channelId (state outputDatum) == cID),
              -- check that the channel funding is maintained. This also checks the integrity of the channel script
              traceIfFalse "wrong output value" correctChannelValue,
              -- check that the channel parameters stay the same
              traceIfFalse "channel parameters differ" (channelParameters oldDatum == channelParameters outputDatum),
              -- check that the time in the output datum is set properly
              traceIfFalse "invalid time in output datum" (allowedValidRangeSize && outputTimeInValidRange),
              -- check that the channel is marked as disputed
              traceIfFalse "failed to mark channel as disputed" (disputed outputDatum)
            ]
    -- Close Case:
    MkClose st ->
      traceIfFalse "wrong input value" correctInputValue
        && traceIfFalse "old state must be funded" (funded oldDatum)
        && let newState = extractVerifiedState st (pSigningPKs $ channelParameters oldDatum)
            in traceIfFalse "Closing state does not belong to this channel" (cID == channelId newState)
                 &&
                 -- check that the state is final
                 traceIfFalse "The closing state is not final" (final newState)
                 &&
                 -- check that A receives their balance
                 traceIfFalse "A party did not get their balance" (all (== True) (zipWith getsValue (pPaymentPKs (channelParameters oldDatum)) (balances newState)))
    -- ForceClose Case:
    ForceClose ->
      traceIfFalse "wrong input value" correctInputValue
        && traceIfFalse "old state must be funded" (funded oldDatum)
        &&
        -- check that there was a prior dispute
        traceIfFalse "try to force close without prior dispute" (disputed oldDatum)
        &&
        -- check that the relative time-lock is past
        traceIfFalse "too early" correctForceCloseSlotRange
        &&
        -- check that A receives their balance
        traceIfFalse "A party did not get their balance" (all (== True) (zipWith getsValue (pPaymentPKs (channelParameters oldDatum)) (balances oldState)))
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    input :: TxInInfo
    input =
      let isScriptInput i = case (txOutDatumHash . txInInfoResolved) i of
            Nothing -> False
            Just _ -> True
          xs = [i | i <- txInfoInputs info, isScriptInput i]
       in case xs of
            [i] -> i
            _ -> traceError "expected exactly one script input"

    inVal :: Value
    inVal = txOutValue . txInInfoResolved $ input

    oldState :: ChannelState
    oldState = state oldDatum

    correctInputValue :: Bool
    correctInputValue = inVal == Ada.lovelaceValueOf (sum $ balances oldState)

    correctInputFunding :: Bool
    correctInputFunding = inVal == Ada.lovelaceValueOf (sum $ funding oldDatum)

    ownOutput :: TxOut
    outputDatum :: ChannelDatum
    (ownOutput, outputDatum) = case getContinuingOutputs ctx of
      [o] -> case txOutDatumHash o of
        Nothing -> traceError "wrong output type"
        Just h -> case findDatum h info of
          Nothing -> traceError "datum not found"
          Just (Datum d) -> case PlutusTx.fromBuiltinData d of
            Just ad' -> (o, ad')
            Nothing -> traceError "error decoding data"
      _ -> traceError "expected exactly one continuing output"

    channelIntegrityAtFunding :: Bool
    channelIntegrityAtFunding =
      channelParameters oldDatum == channelParameters outputDatum
        && state oldDatum == state outputDatum
        && time oldDatum == time outputDatum
        && not (disputed outputDatum)

    correctChannelValue :: Bool
    correctChannelValue =
      txOutValue ownOutput == Ada.lovelaceValueOf (sum $ balances oldState)

    correctChannelFunding :: Bool
    correctChannelFunding =
      txOutValue ownOutput == Ada.lovelaceValueOf (sum $ funding outputDatum)

    checkFundingStatus :: Bool
    checkFundingStatus = (funding outputDatum == balances (state outputDatum)) == funded outputDatum

    getPOSIXStartTime :: LowerBound (Interval POSIXTime) -> POSIXTime
    getPOSIXStartTime (LowerBound (Finite (Interval b _)) _) = getPOSIXTimeFromLowerBound b
    getPOSIXStartTime _ = traceError "unable to verify time"

    getPOSIXTimeFromLowerBound :: LowerBound POSIXTime -> POSIXTime
    getPOSIXTimeFromLowerBound (LowerBound (Finite t) _) = t
    getPOSIXTimeFromLowerBound _ = traceError "unable to verify time"

    getPOSIXEndTime :: UpperBound (Interval POSIXTime) -> POSIXTime
    getPOSIXEndTime (UpperBound (Finite (Interval _ b)) _) = getPOSIXTimeFromUpperBound b
    getPOSIXEndTime _ = traceError "unable to verify time"

    getPOSIXTimeFromUpperBound :: UpperBound POSIXTime -> POSIXTime
    getPOSIXTimeFromUpperBound (UpperBound (Finite t) _) = t
    getPOSIXTimeFromUpperBound _ = traceError "unable to verify time"
    allowedValidRangeSize :: Bool
    allowedValidRangeSize = getPOSIXEndTime (strictUpperBound (txInfoValidRange info)) - getPOSIXStartTime (strictLowerBound (txInfoValidRange info)) <= defaultValidMsRange

    outputTimeInValidRange :: Bool
    outputTimeInValidRange = Ledger.member (time outputDatum) (txInfoValidRange info)

    correctForceCloseSlotRange :: Bool
    correctForceCloseSlotRange = from (time oldDatum + fromMilliSeconds (DiffMilliSeconds (pTimeLock (channelParameters oldDatum)))) `contains` txInfoValidRange info

    -- Returns true if party h is payed value v in an output of the transaction
    getsValue :: PaymentPubKeyHash -> Integer -> Bool
    getsValue pkh v =
      (v == 0)
        || ( let [o] =
                   [ o'
                     | o' <- txInfoOutputs info,
                       txOutValue o' == Ada.lovelaceValueOf v
                   ]
              in -- FIXME is it a problem to assume empty stake part of address here?
                 txOutAddress o == pubKeyHashAddress pkh Nothing
           )

--
-- COMPILATION TO PLUTUS CORE
--
--

typedChannelValidator :: ChannelID -> Scripts.TypedValidator ChannelTypes
typedChannelValidator cID =
  Scripts.mkTypedValidator @ChannelTypes
    ($$(PlutusTx.compile [||mkChannelValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode cID)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @ChannelDatum @ChannelAction

channelValidator :: ChannelID -> Validator
channelValidator = Scripts.validatorScript . typedChannelValidator

channelHash :: ChannelID -> Ledger.ValidatorHash
channelHash = Scripts.validatorHash . typedChannelValidator

channelAddress :: ChannelID -> Ledger.Address
channelAddress = scriptHashAddress . channelHash
