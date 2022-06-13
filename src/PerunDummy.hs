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

--
-- Remove this module header when in Playground
--
module PerunDummy
  ( Channel (..),
    ChannelState (..),
    OpenParams (..),
    DisputeParams (..),
    CloseParams (..),
    ForceCloseParams (..),
    ChannelSchema,
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
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract
import Plutus.Contract.Oracle (SignedMessage (..), SignedMessageCheckError (..), signMessage', verifySignedMessageOnChain)
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential), LedgerBytes (LedgerBytes), StakingCredential (StakingHash), fromBytes)
import qualified PlutusTx
import PlutusTx.Prelude hiding (unless)
import Schema (ToSchema)
import Text.Printf (printf)
import qualified Prelude as P

--
--
-- ON CHAIN PART
--
--

type ChannelID = Integer

--validMsDifference :: Integer
--validMsDifference = 10000
--
defaultValidMsRange :: POSIXTime
defaultValidMsRange = 10000 :: POSIXTime

-- Parameters of the channel
data Channel = Channel
  { pTimeLock :: !Integer,
    pSigningPKA :: !PaymentPubKey,
    pSigningPKB :: !PaymentPubKey,
    pPaymentPKA :: !PaymentPubKeyHash,
    pPaymentPKB :: !PaymentPubKeyHash
  }
  deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

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
  { newState :: !ChannelState,
    sigA :: !(SignedMessage ChannelState),
    sigB :: !(SignedMessage ChannelState)
  }
  deriving (P.Show)

instance Eq SignedState where
  {-# INLINEABLE (==) #-}
  b == c =
    (newState b == newState c)
      && (sigA b == sigA c)
      && (sigB b == sigB c)

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

-- Params:
--      old state :: ChannelState
--      new state :: ChannelState
-- Returns:
--      True, iff the transition from the old state to the new state is valid,
--      i.e. the channelId ist the same, the sum of the balances is the same
--      and the version number strictly increases
{-# INLINEABLE isValidStateTransition #-}
isValidStateTransition :: ChannelState -> ChannelState -> Bool
isValidStateTransition old new =
  (channelId old == channelId new)
    && ((balanceA old + balanceB old) == (balanceA new + balanceB new))
    && (version old < version new)

-- Onchain signature verification.
--verifyOnChain :: BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> Bool
--verifyOnChain = verifySignature

--
-- There is also:
-- signMessage
--    :: ToData a =>
--        a
--        -> PaymentPrivateKey
--        -> Passphrase
--        -> SignedMessage a
--
-- signMessage' == signMessage without `PassPhrase`. All that has to be done is
-- to define a `ToData` and `FromData` instance for whatever has to be signed.

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
      MkDispute SignedState {..} ->
        -- check that the state transition is valid
        traceIfFalse "invalid state transition" (isValidStateTransition oldState (state outputDatum))
          &&
          -- check that the channel id in the state in the dispute matches the actual channel id
          traceIfFalse "state in dispute does not belong to this channel" (channelId (state outputDatum) == cID)
          &&
          -- check that the state in the dispute is reflected in the output datum
          traceIfFalse "output state does not match the state in the dispute" (newState == state outputDatum)
          &&
          -- check that the channel funding is maintained. This also checks the integrity of the channel script
          traceIfFalse "wrong output value" correctChannelFunding
          &&
          -- check that the channel parameters stay the same
          traceIfFalse "channel parameters differ" (channelParameters oldDatum == channelParameters outputDatum)
          &&
          -- check that the time in the output datum is set properly
          traceIfFalse "invalid time in output datum" (allowedValidRangeSize && outputTimeInValidRange)
          &&
          -- check that the channel is marked as disputed
          traceIfFalse "failed to mark channel as disputed" (disputed outputDatum)
          &&
          -- check that A's supplied key is correct
          -- traceIfFalse "A's supplied keys do not match their key in the channel parameters" (verifyKeysMatch keyA (pPartyA (channelParameters oldDatum)))
          -- &&
          -- check that B's supplied key is correct
          -- traceIfFalse "B's supplied keys do not match their key in the channel parameters" (verifyKeysMatch keyB (pPartyB (channelParameters oldDatum)))
          -- &&
          -- check that A's signature on the new state is valid
          traceIfFalse "A's signed state does not match the state in the dispute" (getStateFromValidSignature (pSigningPKA (channelParameters oldDatum)) sigA == newState)
          &&
          -- check that B's signature on the new state is valid
          traceIfFalse "B's signed state does not match the state in the dispute" (getStateFromValidSignature (pSigningPKB (channelParameters oldDatum)) sigB == newState)
      -- Close Case
      MkClose SignedState {..} ->
        --
        traceIfFalse "Closing state does not belong to this channel" (cID == channelId newState)
          &&
          -- check that A's supplied key is correct
          --traceIfFalse "A's supplied keys do not match their key in the channel parameters" (verifyKeysMatch keyA (pPartyA (channelParameters oldDatum)))
          -- &&
          -- check that B's supplied key is correct
          --traceIfFalse "B's supplied keys do not match their key in the channel parameters" (verifyKeysMatch keyB (pPartyB (channelParameters oldDatum)))
          -- &&
          -- check that A's signature on the new state is valid
          traceIfFalse "A's signed state does not match the state in the dispute" (getStateFromValidSignature (pSigningPKA (channelParameters oldDatum)) sigA == newState)
          &&
          -- check that B's signature on the new state is valid
          traceIfFalse "B's signed state does not match the state in the dispute" (getStateFromValidSignature (pSigningPKB (channelParameters oldDatum)) sigB == newState)
          &&
          -- check that the state is final
          traceIfFalse "The closing state is not final" (final newState)
          &&
          -- check that A receives their balance
          traceIfFalse "Party A did not get their balance" (getsValue (pPaymentPKA (channelParameters oldDatum)) $ Ada.lovelaceValueOf (balanceA newState))
          &&
          -- check that B receives their balance
          traceIfFalse "Party B did not get their balance" (getsValue (pPaymentPKB (channelParameters oldDatum)) $ Ada.lovelaceValueOf (balanceB newState))
      -- ForceClose Case
      ForceClose ->
        -- check that there was a prior dispute
        traceIfFalse "try to force close without prior dispute" (disputed oldDatum)
          &&
          -- check that the relative time-lock is past
          traceIfFalse "too early" correctForceCloseSlotRange
          &&
          -- check that Party A receives their balance
          traceIfFalse "Party A did not get their balance" (getsValue (pPaymentPKA (channelParameters oldDatum)) $ Ada.lovelaceValueOf (balanceA oldState))
          &&
          -- check that Party B receives their balance
          traceIfFalse "Party B did not get their balance" (getsValue (pPaymentPKB (channelParameters oldDatum)) $ Ada.lovelaceValueOf (balanceB oldState))
  where
    --- The out-scripts view of the transaction body of the consuming transaction
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
    correctInputValue = inVal == Ada.lovelaceValueOf (balanceA oldState + balanceB oldState)

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

    --- Check that the output of the bidding transaction maintains the channel funding
    correctChannelFunding :: Bool
    correctChannelFunding =
      txOutValue ownOutput == Ada.lovelaceValueOf (balanceA oldState + balanceB oldState)

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

    getStateFromValidSignature :: PaymentPubKey -> SignedMessage ChannelState -> ChannelState
    getStateFromValidSignature key sm =
      let x = verifySignedMessageOnChain ctx key sm
       in case x of
            Left SignatureMismatch {} -> traceError "Signedstate error: SignatureMismatch on signed state"
            Left DatumMissing {} -> traceError "Signedstate error: Missing datum in tx"
            Left DecodingError -> traceError "Signedstate error: Malformed Datum"
            Left DatumNotEqualToExpected -> traceError "SignedState error: Datum not according to hash"
            Right state -> state

    --verifyKeysMatch :: PaymentPubKey -> PaymentPubKeyHash -> Bool
    --verifyKeysMatch pk pkh = paymentPubKeyHash pk == pkh

    allowedValidRangeSize :: Bool
    allowedValidRangeSize = (getPOSIXEndTime (strictUpperBound (txInfoValidRange info)) - getPOSIXStartTime (strictLowerBound (txInfoValidRange info))) <= defaultValidMsRange

    -- Check that the time in the output datum of the consuming transaction is located inside the valid range of the consuming transaction
    outputTimeInValidRange :: Bool
    outputTimeInValidRange = Ledger.member (time outputDatum) (txInfoValidRange info)

    -- Check that the relative time-lock has passed
    correctForceCloseSlotRange :: Bool
    correctForceCloseSlotRange = from (time oldDatum + fromMilliSeconds (DiffMilliSeconds (pTimeLock (channelParameters oldDatum)))) `contains` txInfoValidRange info

    -- Returns true if party h is payed value v in an output of the transaction
    getsValue :: PaymentPubKeyHash -> Value -> Bool
    getsValue pkh v =
      let [o] =
            [ o'
              | o' <- txInfoOutputs info,
                txOutValue o' == v
            ]
       in -- inlined code from Ledger.Address pubKeyAddress :: PaymentPubKey -> Maybe StakePubKeyHash-> Address
          -- then switched (pubKeyHash key) to dummy -> then it compiles why tho?!?!?
          txOutAddress o == pubKeyHashAddress pkh Nothing

--
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
    spSigningPKA :: !PaymentPubKey,
    spSigningPKB :: !PaymentPubKey,
    spPaymentPKA :: !PaymentPubKeyHash,
    spPaymentPKB :: !PaymentPubKeyHash,
    spBalanceA :: !Integer,
    spBalanceB :: !Integer,
    spTimeLock :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)
  deriving stock (P.Eq, P.Show)

data DisputeParams = DisputeParams
  { dpChannelId :: !ChannelID,
    dpBalanceA :: !Integer,
    dpBalanceB :: !Integer,
    dpVersion :: !Integer,
    dpFinal :: !Bool,
    dpSigA :: !(SignedMessage ChannelState),
    dpSigB :: !(SignedMessage ChannelState)
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

data CloseParams = CloseParams
  { cpChannelId :: !ChannelID,
    cpBalanceA :: !Integer,
    cpBalanceB :: !Integer,
    cpVersion :: !Integer,
    cpFinal :: !Bool,
    cpSigA :: !(SignedMessage ChannelState),
    cpSigB :: !(SignedMessage ChannelState)
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

newtype ForceCloseParams = ForceCloseParams ChannelID
  deriving newtype (ToJSON, FromJSON)
  deriving (Generic)
  deriving stock (P.Eq, P.Show)

type ChannelSchema =
  Endpoint "open" OpenParams
    .\/ Endpoint "dispute" DisputeParams
    .\/ Endpoint "close" CloseParams
    .\/ Endpoint "forceClose" ForceCloseParams

--
-- open channel
--

-- sets the transaction values for forming the initial auction transaction (endpoint start)
open :: AsContractError e => OpenParams -> Contract w s e ()
open OpenParams {..} = do
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
  ledgerTx <- submitTxConstraints (typedChannelValidator spChannelId) tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $ printf "Opened channel %d with parameters %s and value %s" spChannelId (P.show c) (P.show v)

--
-- dispute logic
--

dispute :: forall w s. DisputeParams -> Contract w s Text ()
dispute DisputeParams {..} = do
  t <- currentTime
  (oref, o, d@ChannelDatum {..}) <- findChannel dpChannelId
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  when (dpVersion <= version state) $
    throwError $ pack $ printf "version no greater than current version number"
  let s =
        ChannelState
          { channelId = dpChannelId,
            balanceA = dpBalanceA,
            balanceB = dpBalanceB,
            version = dpVersion,
            final = dpFinal
          }
      disp =
        SignedState
          { newState = s,
            sigA = dpSigA,
            sigB = dpSigB
          }
      newDatum =
        d
          { state = s,
            time = t,
            disputed = True
          }
      v = Ada.lovelaceValueOf (balanceA state + balanceB state)
      r = Redeemer $ PlutusTx.toBuiltinData $ MkDispute disp

      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator dpChannelId)
          P.<> Constraints.otherScript (channelValidator dpChannelId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        Constraints.mustPayToTheScript newDatum v
          <> Constraints.mustValidateIn (Ledger.intersection (from (t - 1000)) (to (t + defaultValidMsRange - 1000)))
          <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $ printf "made dispute of new state %s" (P.show s)

--
-- close logic
--

close :: forall w s. CloseParams -> Contract w s Text ()
close CloseParams {..} = do
  (oref, o, d@ChannelDatum {..}) <- findChannel cpChannelId
  logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
  unless cpFinal $
    throwError $ pack $ printf "can not close unless state is final"
  let s =
        ChannelState
          { channelId = cpChannelId,
            balanceA = cpBalanceA,
            balanceB = cpBalanceB,
            version = cpVersion,
            final = cpFinal
          }
      cls =
        SignedState
          { newState = s,
            sigA = cpSigA,
            sigB = cpSigB
          }
      r = Redeemer $ PlutusTx.toBuiltinData $ MkClose cls
      lookups =
        Constraints.typedValidatorLookups (typedChannelValidator cpChannelId)
          P.<> Constraints.otherScript (channelValidator cpChannelId)
          P.<> Constraints.unspentOutputs (Map.singleton oref o)
      tx =
        Constraints.mustPayToPubKey (pPaymentPKA channelParameters) (Ada.lovelaceValueOf cpBalanceA)
          <> Constraints.mustPayToPubKey (pPaymentPKB channelParameters) (Ada.lovelaceValueOf cpBalanceB)
          <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $
    printf
      "closed channel %d with params %s. The final balance is: (%d, %d)"
      cpChannelId
      (P.show channelParameters)
      cpBalanceA
      cpBalanceB

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
        Constraints.mustPayToPubKey (pPaymentPKA channelParameters) (Ada.lovelaceValueOf (balanceA state))
          <> Constraints.mustPayToPubKey (pPaymentPKB channelParameters) (Ada.lovelaceValueOf (balanceB state))
          <> Constraints.mustValidateIn (from (time + 1 + fromMilliSeconds (DiffMilliSeconds (pTimeLock channelParameters))))
          <> Constraints.mustSpendScriptOutput oref r
  ledgerTx <- submitTxConstraintsWith lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  logInfo @P.String $
    printf
      "force closed channel %d with parameters %s. The final balance is: (%d, %d)"
      cId
      (P.show channelParameters)
      (balanceA state)
      (balanceB state)

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

-- TODO this is a helper just for testing, move this appropriately
signState :: PrivateKey -> PrivateKey -> ChannelState -> SignedState
signState keyA keyB state = SignedState {newState = state, sigA = signMessage' state keyA, sigB = signMessage' state keyB}
