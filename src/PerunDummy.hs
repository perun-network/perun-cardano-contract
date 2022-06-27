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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use &&" #-}

--
-- Remove this module header when in Playground
--
module PerunDummy
  ( Channel (..),
    ChannelState (..),
    SignedState (..),
    OpenParams (..),
    DisputeParams (..),
    CloseParams (..),
    ForceCloseParams (..),
    ChannelSchema,
    ChannelTypes,
    ChannelAction,
    ChannelDatum,
    open,
    dispute,
    close,
    forceClose,
    contract,
    ensureKnownCurrencies,
    printJson,
    printSchemas,
    stage,
  )
where

--
-- end
--
import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data
import Data.Map as Map
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Ledger hiding (singleton)
import Ledger.Ada as Ada
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract (ensureKnownCurrencies, printJson, printSchemas, stage)
import Plutus.Contract
import Plutus.Contract.Oracle (SignedMessage (..), SignedMessageCheckError (..), verifySignedMessageConstraints)
import qualified PlutusTx
import PlutusTx.Prelude hiding (unless)
import Schema (ToSchema)
import Text.Printf (printf)
import qualified Prelude as P
import Verifier.Honest
import Types

--
--
-- ON CHAIN PART
--
--

defaultValidMsRange :: POSIXTime
defaultValidMsRange = 10000

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
    (pTimeLock a == pTimeLock b)
      && (pSigningPKs a == pSigningPKs b)
      && (pPaymentPKs a == pPaymentPKs b)

PlutusTx.unstableMakeIsData ''Channel
PlutusTx.makeLift ''Channel

data ChannelState = ChannelState
  { channelId :: !ChannelID,
    balances :: ![Integer],
    version :: !Integer,
    final :: !Bool
  }
  deriving (Data)
  deriving stock (P.Eq, P.Show)

instance Eq ChannelState where
  {-# INLINEABLE (==) #-}
  b == c =
    (channelId b == channelId c)
      && (balances b == balances c)
      && (version b == version c)
      && (final b == final c)

PlutusTx.unstableMakeIsData ''ChannelState
PlutusTx.makeLift ''ChannelState

data SignedState = SignedState
  { stateSigs :: ![SignedMessage ChannelState]
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

instance Eq SignedState where
  {-# INLINEABLE (==) #-}
  a == b = stateSigs a == stateSigs b



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

-- | Returns true, iff the new state is a valid post-state of the old channel state.
-- | A valid state transition must retain the channelId and the sum of the balances.
-- | The new version number must be greater than the old version number and there is
-- | no valid transition from a final state.
{-# INLINEABLE isValidStateTransition #-}
isValidStateTransition :: ChannelState -> ChannelState -> Bool
isValidStateTransition old new =
  (channelId old == channelId new)
    && (sum (balances old) == sum (balances new))
    && (version old < version new)
    && not (final old)


-- | pls work
{-# INLINEABLE extractVerifiedState #-}
extractVerifiedState :: SignedState -> [PaymentPubKey] -> ChannelState
extractVerifiedState (SignedState sigs) signingKeys =
  let states = zipWith verifySignedMessageConstraints' signingKeys sigs in
  if and $ PlutusTx.Prelude.map (== head states) (tail states) then
    (case head states of
      Just s -> s
      Nothing -> traceError "invalid signatures") else
        traceError "invalidSignatures"


-- | Checks all signatures on the given SignedState under the given public keys
-- | and returns the corresponding ChannelState if all signatures are valid on 
-- | the same ChannelState.
--{-# INLINEABLE extractVerifiedState #-}
--extractVerifiedState :: SignedState -> (PaymentPubKey, PaymentPubKey) -> ChannelState
--extractVerifiedState (SignedState smA smB) (pkA, pkB) = case (verifySignedMessageConstraints' pkA smA, verifySignedMessageConstraints' pkB smB) of
--  (Right (sa, _), Right (sb, _)) -> if sa == sb then sa else traceError "signatures on different states"
--  _ -> traceError "invalid signatures"

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
  traceIfFalse "wrong input value" correctInputValue
    && case action of
      -- Dispute Case:
      MkDispute st ->
        let newState = extractVerifiedState st (pSigningPKs $ channelParameters oldDatum)
         in and
              [ -- check that the state in the dispute is reflected in the output datum
                traceIfFalse "output state does not match the state in the dispute" (newState == state outputDatum),
                -- check that the state transition is valid
                traceIfFalse "invalid state transition" (isValidStateTransition oldState (state outputDatum)),
                -- check that the channel id in the state in the dispute matches the actual channel id
                traceIfFalse "state in dispute does not belong to this channel" (channelId (state outputDatum) == cID),
                -- check that the channel funding is maintained. This also checks the integrity of the channel script
                traceIfFalse "wrong output value" correctChannelFunding,
                -- check that the channel parameters stay the same
                traceIfFalse "channel parameters differ" (channelParameters oldDatum == channelParameters outputDatum),
                -- check that the time in the output datum is set properly
                traceIfFalse "invalid time in output datum" (allowedValidRangeSize && outputTimeInValidRange),
                -- check that the channel is marked as disputed
                traceIfFalse "failed to mark channel as disputed" (disputed outputDatum)
              ]
      -- Close Case:
      MkClose st ->
        let newState = extractVerifiedState st (pSigningPKs $ channelParameters oldDatum)
         in and
              [ traceIfFalse "Closing state does not belong to this channel" (cID == channelId newState),
                -- check that the state is final
                traceIfFalse "The closing state is not final" (final newState),
                -- check that A receives their balance
                traceIfFalse "A party did not get their balance" (all (== True) (zipWith getsValue (pPaymentPKs (channelParameters oldDatum)) (balances newState)))
              ]
      -- ForceClose Case:
      ForceClose ->
        and
          [ -- check that there was a prior dispute
            traceIfFalse "try to force close without prior dispute" (disputed oldDatum),
            -- check that the relative time-lock is past
            traceIfFalse "too early" correctForceCloseSlotRange,
            -- check that A receives their balance
            traceIfFalse "A party did not get their balance" (all (== True) (zipWith getsValue (pPaymentPKs (channelParameters oldDatum)) (balances oldState)))
          ]
  where
    -- | The out-scripts view of the transaction body of the consuming transaction
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

    -- | Check that the output of the bidding transaction maintains the channel funding
    correctChannelFunding :: Bool
    correctChannelFunding =
      txOutValue ownOutput == Ada.lovelaceValueOf (sum $ balances oldState)

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

    -- | Check that the consuming transaction is valid in an interval of at most `defaultValidMsRange` milliseconds
    allowedValidRangeSize :: Bool
    allowedValidRangeSize = (getPOSIXEndTime (strictUpperBound (txInfoValidRange info)) - getPOSIXStartTime (strictLowerBound (txInfoValidRange info))) <= defaultValidMsRange

    -- | Check that the time in the output datum of the consuming transaction is located inside the valid range of the consuming transaction
    outputTimeInValidRange :: Bool
    outputTimeInValidRange = Ledger.member (time outputDatum) (txInfoValidRange info)

    -- | Check that the relative time-lock has passed
    correctForceCloseSlotRange :: Bool
    correctForceCloseSlotRange = from (time oldDatum + fromMilliSeconds (DiffMilliSeconds (pTimeLock (channelParameters oldDatum)))) `contains` txInfoValidRange info

    -- Returns true if party h is payed value v in an output of the transaction
    getsValue :: PaymentPubKeyHash -> Integer -> Bool
    getsValue pkh v =
      let [o] =
            [ o'
              | o' <- txInfoOutputs info,
                txOutValue o' == Ada.lovelaceValueOf v
            ]
       in txOutAddress o == pubKeyHashAddress pkh Nothing

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


type ChannelSchema =
  Endpoint "open" OpenParams
    .\/ Endpoint "dispute" DisputeParams
    .\/ Endpoint "close" CloseParams
    .\/ Endpoint "forceClose" ForceCloseParams

--
-- open channel
--

mockOpen :: AsContractError e => MockParams -> Contract w s e ()
mockOpen MockParams {..} = do
  let c =
        Channel
          { pTimeLock = mockTimeLock,
            pSigningPKA = mockSigningPKA,
            pSigningPKB = mockSigningPKB,
            pPaymentPKA = mockPaymentPKA,
            pPaymentPKB = mockPaymentPKB
          }
      s =
        ChannelState
          { channelId = mockChannelId,
            balanceA = mockBalanceA,
            balanceB = mockBalanceB,
            version = mockVersion,
            final = mockFinal
          }
      d =
        ChannelDatum
          { channelParameters = c,
            state = s,
            time = mockTimeStamp,
            disputed = mockDisputed
          }
      v = Ada.lovelaceValueOf mockValue
      tx = Constraints.mustPayToTheScript d v
  ledgerTx <- submitTxConstraints (typedChannelValidator mockChannelId) tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $ printf "Opened channel %d with parameters %s and value %s" mockChannelId (P.show c) (P.show v)

mockDispute :: forall w s. MockParams -> Contract w s Text ()
mockDispute MockParams {..} = do
  (oref, o, d) <- findChannel  mockChannelId
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  sst <- case mockSignedState of
    Nothing -> throwError "missing signed state"
    Just s -> return s
  let c =
        Channel
          { pTimeLock = mockTimeLock,
            pSigningPKA = mockSigningPKA,
            pSigningPKB = mockSigningPKB,
            pPaymentPKA = mockPaymentPKA,
            pPaymentPKB = mockPaymentPKB
          }
      s =
        ChannelState
          { channelId = mockChannelId,
            balanceA = mockBalanceA,
            balanceB = mockBalanceB,
            version = mockVersion,
            final = mockFinal
          }
      datum =
        ChannelDatum
          { channelParameters = c,
            state = s,
            time = mockTimeStamp,
            disputed = mockDisputed
          }
      v = Ada.lovelaceValueOf mockValue
      
      r = Redeemer $ PlutusTx.toBuiltinData $ MkDispute sst
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator mockChannelId)
          P.<> Constraints.otherScript (channelValidator mockChannelId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        Constraints.mustPayToTheScript datum v
          <> Constraints.mustValidateIn (fromMaybe (always :: Interval POSIXTime)  mockValidTimeRange)
          <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $ printf "made dispute of new state %s" (P.show s)


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
            disputed = False
          }
      v = Ada.lovelaceValueOf $ sum spBalances
      tx = Constraints.mustPayToTheScript d v
  ledgerTx <- submitTxConstraints (typedChannelValidator spChannelId) tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $ printf "Opened channel %d with parameters %s and value %s" spChannelId (P.show c) (P.show v)

--
-- dispute logic
--

dispute :: forall w s. DisputeParams -> Contract w s Text ()
dispute (DisputeParams keys sst) = do
  let
    dState = extractVerifiedState sst keys
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
      v = Ada.lovelaceValueOf $ sum $ balances state
      r = Redeemer $ PlutusTx.toBuiltinData $ MkDispute sst

      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator $ channelId dState)
          P.<> Constraints.otherScript (channelValidator $ channelId dState)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        Constraints.mustPayToTheScript newDatum v
          <> Constraints.mustValidateIn (Ledger.intersection (from (t - 1000)) (to (t + defaultValidMsRange - 1000)))
          <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $ printf "made dispute of new state %s" (P.show dState)

--
-- close logic
--

close :: forall w s. CloseParams -> Contract w s Text ()
close (CloseParams keys sst) = do
  let
    s@ChannelState{..} = extractVerifiedState sst keys
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
      mconcat (zipWith Constraints.mustPayToPubKey (pPaymentPKs channelParameters) (PlutusTx.Prelude.map Ada.lovelaceValueOf balances))
        <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $
    printf
      "closed channel %d with params %s. The final balances are: %s"
      channelId
      (P.show channelParameters)
      (P.show balances)

--
-- close logic
--

forceClose :: forall w s. ForceCloseParams -> Contract w s Text ()
forceClose (ForceCloseParams cId) = do
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
        mconcat (zipWith Constraints.mustPayToPubKey (pPaymentPKs channelParameters) (PlutusTx.Prelude.map Ada.lovelaceValueOf (balances state)))
          <> Constraints.mustValidateIn (from (time + 1 + fromMilliSeconds (DiffMilliSeconds (pTimeLock channelParameters))))
          <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $
    printf
      "force closed channel %d with parameters %s. The final balance are: %s"
      cId
      (P.show channelParameters)
      (P.show $ balances state)

findChannel ::
  ChannelID ->
  Contract w s Text (TxOutRef, ChainIndexTxOut, ChannelDatum)
findChannel cID = do
  utxos <- utxosAt $ scriptHashAddress (channelHash cID)
  let xs =
        [ (oref, o)
          | (oref, o) <- Map.toList utxos
        ]
  case xs of
    [(oref, o)] -> case _ciTxOutDatum o of
      Left _ -> throwError "datum missing"
      Right (Datum e) -> case PlutusTx.fromBuiltinData e of
        Nothing -> throwError "datum has wrong type"
        Just d@ChannelDatum {} -> return (oref, o, d)
    _ -> throwError "channel utxo not found"

--
-- Top-level contract, exposing all endpoints.
--
contract :: Contract () ChannelSchema Text ()
contract = selectList [open', dispute', close', forceClose'] >> contract
  where
    open' = endpoint @"open" open
    dispute' = endpoint @"dispute" dispute
    close' = endpoint @"close" close
    forceClose' = endpoint @"forceClose" forceClose
