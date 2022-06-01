{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

--
-- Remove this module header when in Playground
--
module PerunDummy
    ( Channel (..)
    , OpenParams (..), DisputeParams (..), CloseParams (..), ForceCloseParams(..),
    , ChannelSchema
    , open, dispute, close, forceClose
    , endpoints
    , schemas
    , ensureKnownCurrencies
    , printJson
    , printSchemas
    , registeredKnownCurrencies
    , stage
    ) where
--
-- end
--
import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada
import           Playground.Contract  (IO, ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (unless)
import qualified Prelude              as P
import           Schema               (ToSchema)
import           Text.Printf          (printf)


--
--
-- ON CHAIN PART
--
--


--validMsDifference :: Integer
--validMsDifference = 10000

defaultValidMsRange :: POSIXTime
defaultValidMsRange = (10000 :: POSIXTime)

-- Parameters of the channel
data Channel = Channel
    { pChannelId   :: !Integer
    , pTimeLock :: !Integer
    , pPartyA   :: !PaymentPubKeyHash
    , pPartyB :: !PaymentPubKeyHash
    } deriving (P.Show, Generic, ToJSON, FromJSON, ToSchema)

-- Equality of two Channels
instance Eq Channel where
    {-# INLINABLE (==) #-}
    a == b = (pChannelId   a == pChannelId   b) &&
             (pTimeLock a == pTimeLock b) &&
             (pPartyA   a == pPartyA   b) &&
             (pPartyB a == pPartyB b)


PlutusTx.unstableMakeIsData ''Channel
PlutusTx.makeLift ''Channel

data State = State
    { channelId  :: !Integer
    , balanceA :: !Integer
    , balanceB :: !Integer
    , version :: !Integer
    } deriving P.Show

instance Eq State where
    {-# INLINABLE (==) #-}
    b == c = (channelId b == channelId  c) &&
             (balanceA  b == balanceA   c) &&
             (balanceB  b == balanceB   c) &&
             (version   b == version    c)

PlutusTx.unstableMakeIsData ''State
PlutusTx.makeLift ''State



data Dispute = Dispute
    { newState :: !State
    } deriving P.Show

instance Eq Dispute where
    {-# INLINABLE (==) #-}
    b == c = (newState b == newState c)

PlutusTx.unstableMakeIsData ''Dispute
PlutusTx.makeLift ''Dispute

-- Redeemer Datatype
data ChannelAction = MkDispute Dispute | Close | ForceClose
    deriving P.Show

PlutusTx.unstableMakeIsData ''ChannelAction
PlutusTx.makeLift ''ChannelAction

-- Datum datatype
data ChannelDatum = ChannelDatum
    { channelParameters   :: !Channel
    , state     :: !State
    , time      :: !Ledger.POSIXTime
    , disputed :: !Bool
    } deriving P.Show

PlutusTx.unstableMakeIsData ''ChannelDatum
PlutusTx.makeLift ''ChannelDatum

-- Boilerplate code that allows us to use our custom types for Datum and Redeemer in the Validator script instead of BuiltinData
data ChannelTypes
instance Scripts.ValidatorTypes ChannelTypes where
    type instance RedeemerType ChannelTypes = ChannelAction
    type instance DatumType ChannelTypes = ChannelDatum

-- Params:
--      old state :: State
--      new state :: State
-- Returns:
--      True, iff the transition from the old state to the new state is valid, i.e. the channelId ist the same, the sum of the balances is the same
--      and the version number strictly increases
{-# INLINABLE isValidStateTransition #-}
isValidStateTransition :: State -> State -> Bool
isValidStateTransition old new =    (channelId old == channelId new) &&
                                    (((balanceA old) + (balanceB old)) == ((balanceA new) + (balanceB new))) &&
                                    (version old < version new)

-- Params:
--  Datum
--  Redeemer
--  Consuming Transaction
-- Returns:
--  Bool: Transaction is valid, iff this returns true and no errors are thrown
{-# INLINABLE mkChannelValidator #-}
mkChannelValidator :: ChannelDatum -> ChannelAction -> ScriptContext -> Bool
mkChannelValidator oldDatum action ctx =
    traceIfFalse "wrong input value" correctInputValue &&
    case action of
    -- Dispute Case:
        MkDispute d@Dispute{..} ->
            -- check that the state transition is valid
            traceIfFalse "invalid state transition" (isValidStateTransition oldState (state outputDatum)) &&
            -- check that the state in the dispute is reflected in the output datum
            traceIfFalse "output state does not match the state in the dispute"  (newState == (state outputDatum)) &&
            -- check that the channel funding is maintained. This also checks the integrity of the channel script
            traceIfFalse "wrong output value" correctChannelFunding &&
            -- check that the channel parameters stay the same
            traceIfFalse "channel parameters differ" ((channelParameters oldDatum) == (channelParameters outputDatum)) &&
            -- check that the time in the output datum is set properly
            traceIfFalse "invalid time in output datum" (outputTimeInValidRange) &&
            -- check that the channel is marked as disputed
            traceIfFalse "failed to mark channel as disputed" (disputed outputDatum)
    -- Close Case
        Close           ->
            -- Usually, we would need a signature check here!
            True
    -- ForceClose Case
        ForceClose      ->
            -- check that there was a prior dispute
            traceIfFalse "try to force close without prior dispute" (disputed oldDatum) &&
            -- check that the relative time-lock is past
            traceIfFalse "too early" correctForceCloseSlotRange &&
            -- check that Party A receives their balance
            traceIfFalse "Party A did not get their balance" (getsValue (pPartyA (channelParameters oldDatum)) $ Ada.lovelaceValueOf (balanceA oldState)) &&
            -- check that Party B receives their balance
            traceIfFalse "Party B did not get their balance" (getsValue (pPartyB (channelParameters oldDatum)) $ Ada.lovelaceValueOf (balanceB oldState))
  where
--- The out-scripts view of the transaction body of the consuming transaction
    info :: TxInfo
    info = scriptContextTxInfo ctx

    input :: TxInInfo
    input =
      let
        isScriptInput i = case (txOutDatumHash . txInInfoResolved) i of
            Nothing -> False
            Just _  -> True
        xs = [i | i <- txInfoInputs info, isScriptInput i]
      in
        case xs of
            [i] -> i
            _   -> traceError "expected exactly one script input"

    inVal :: Value
    inVal = txOutValue . txInInfoResolved $ input

    oldState :: State
    oldState = state oldDatum

    correctInputValue :: Bool
    correctInputValue = inVal == Ada.lovelaceValueOf ((balanceA oldState) + (balanceB oldState))

    ownOutput   :: TxOut
    outputDatum :: ChannelDatum
    (ownOutput, outputDatum) = case getContinuingOutputs ctx of
        [o] -> case txOutDatumHash o of
            Nothing   -> traceError "wrong output type"
            Just h -> case findDatum h info of
                Nothing        -> traceError "datum not found"
                Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
                    Just ad' -> (o, ad')
                    Nothing  -> traceError "error decoding data"
        _   -> traceError "expected exactly one continuing output"


--- Check that the output of the bidding transaction maintains the channel funding
    correctChannelFunding :: Bool
    correctChannelFunding =
        txOutValue ownOutput == Ada.lovelaceValueOf ((balanceA oldState) + (balanceB oldState))

    getPOSIXStartTime :: LowerBound (Interval POSIXTime) -> POSIXTime
    getPOSIXStartTime (LowerBound (Finite (Interval b _)) _) = getPOSIXTimeFromLowerBound b
    getPOSIXStartTime _ = traceError "unable to verify time"

    getPOSIXTimeFromLowerBound :: (LowerBound POSIXTime) -> POSIXTime
    getPOSIXTimeFromLowerBound (LowerBound (Finite t) _) = t
    getPOSIXTimeFromLowerBound _ = traceError "unable to verify time"

    getPOSIXEndTime :: UpperBound (Interval POSIXTime) -> POSIXTime
    getPOSIXEndTime (UpperBound (Finite (Interval _ b)) _) = getPOSIXTimeFromUpperBound b
    getPOSIXEndTime _ = traceError "unable to verify time"

    getPOSIXTimeFromUpperBound :: (UpperBound POSIXTime) -> POSIXTime
    getPOSIXTimeFromUpperBound (UpperBound (Finite t) _) = t
    getPOSIXTimeFromUpperBound _ = traceError "unable to verify time"


--- TODO: How can we check that the difference between the end of txInfoValidRange and the time specified in the datum is not too big???
---         There seems to be no width function for intervals ...

--- Check that the valid range of the consuming transaction is not bigger than some allowed margin
---    allowedValidRangeEnd :: Bool
---    allowedValidRangeEnd = (width (intersection (from (time outputDatum)) (txInfoValidRange info))) <= validMsDifference

    allowedValidRangeSize :: Bool
    allowedValidRangeSize = ((getPOSIXEndTime (strictUpperBound (txInfoValidRange info))) - (getPOSIXStartTime (strictLowerBound (txInfoValidRange info)))) <= defaultValidMsRange

--- Check that the time in the output datum of the consuming transaction is located inside the valid range of the consuming transaction
    outputTimeInValidRange :: Bool
    outputTimeInValidRange = Ledger.member (time outputDatum) (txInfoValidRange info)

--- Check that the relative time-lock has passed
    correctForceCloseSlotRange :: Bool
    correctForceCloseSlotRange = (from ((time oldDatum) + (fromMilliSeconds (DiffMilliSeconds (pTimeLock (channelParameters oldDatum))))))  `contains` txInfoValidRange info

--- Returns true if party h is payed value v in an output of the transaction
    getsValue :: PaymentPubKeyHash -> Value -> Bool
    getsValue h v =
      let
        [o] = [ o'
              | o' <- txInfoOutputs info
              , txOutValue o' == v
              ]
      in
        txOutAddress o == pubKeyHashAddress h Nothing

--
--
-- COMPILATION TO PLUTUS CORE
--
--

typedChannelValidator :: Scripts.TypedValidator ChannelTypes
typedChannelValidator = Scripts.mkTypedValidator @ChannelTypes
    $$(PlutusTx.compile [|| mkChannelValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @ChannelDatum @ChannelAction

channelValidator :: Validator
channelValidator = Scripts.validatorScript typedChannelValidator

channelHash :: Ledger.ValidatorHash
channelHash = Scripts.validatorHash typedChannelValidator

channelAddress :: Ledger.Address
channelAddress = scriptHashAddress channelHash

--
--
-- OFF-CHAIN PART
--
--

--
-- Parameters for Endpoints, that can then be invoked
--

data OpenParams = OpenParams
    { spChannelId :: !Integer
    , spPartyA :: !PaymentPubKeyHash
    , spPartyB :: !PaymentPubKeyHash
    , spBalanceA :: !Integer
    , spBalanceB :: !Integer
    , spTimeLock :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data DisputeParams = DisputeParams
    { dpChannelId :: !Integer
    , dpBalanceA  :: !Integer
    , dpBalanceB  :: !Integer
    , dpVersion   :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data CloseParams = CloseParams
    { cpChannelId :: !Integer
    , cpBalanceA :: !Integer
    , cpBalanceB :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

data ForceCloseParams = ForceCloseParams
    { fpChannelId :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type ChannelSchema =
        Endpoint "open" OpenParams
    .\/ Endpoint "dispute"   DisputeParams
    .\/ Endpoint "close" CloseParams
    .\/ Endpoint "forceClose" ForceCloseParams

--
-- open channel
--

-- sets the transaction values for forming the initial auction transaction (endpoint start)
open :: AsContractError e => OpenParams -> Contract w s e ()
open OpenParams{..} = do
    t <- currentTime
    let c = Channel
                { pChannelId = spChannelId
                , pTimeLock = spTimeLock
                , pPartyA = spPartyA
                , pPartyB = spPartyB
                }
        s = State
                { channelId = spChannelId
                , balanceA = spBalanceA
                , balanceB = spBalanceB
                , version = 0
                }
        d = ChannelDatum
                { channelParameters = c
                , state = s
                , time = t
                , disputed = False
                }
        v = Ada.lovelaceValueOf (spBalanceA + spBalanceB)
        tx = Constraints.mustPayToTheScript d v
    ledgerTx <- submitTxConstraints typedChannelValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "Opened channel %s with value %s" (P.show c) (P.show v)

--
-- dispute logic
--

dispute :: forall w s. DisputeParams -> Contract w s Text ()
dispute DisputeParams{..} = do
    t <- currentTime
    (oref, o, d@ChannelDatum{..}) <- findChannel dpChannelId
    logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
--
-- function minBid used here in off-chain code, but defined in on-chain code
-- (Example of code reuse)
--
    when (dpVersion <= (version state)) $
        throwError $ pack $ printf "version no greater than current version number"
    let s  = State  { channelId = dpChannelId
                    , balanceA = dpBalanceA
                    , balanceB = dpBalanceB
                    , version = dpVersion
                    }
        disp  = Dispute {newState = s}
        newDatum = d {  state = s
                        , time = t
                        , disputed = True
                    }
        v  = Ada.lovelaceValueOf ((balanceA state) + (balanceB state))
        r  = Redeemer $ PlutusTx.toBuiltinData $ MkDispute disp

        lookups = Constraints.typedValidatorLookups typedChannelValidator P.<>
                  Constraints.otherScript channelValidator                P.<>
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx      =   Constraints.mustPayToTheScript newDatum v                            <>
                    Constraints.mustValidateIn (Ledger.intersection (from (t - 1000)) (to (t + defaultValidMsRange - 1000)))     <>
                    Constraints.mustSpendScriptOutput oref r
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "made dispute of new state %s" (P.show s)

--
-- close logic
--

close :: forall w s. CloseParams -> Contract w s Text ()
close CloseParams{..} = do
    (oref, o, d@ChannelDatum{..}) <- findChannel cpChannelId
    logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
    let r       = Redeemer $ PlutusTx.toBuiltinData Close
        lookups = Constraints.typedValidatorLookups typedChannelValidator P.<>
                  Constraints.otherScript channelValidator                P.<>
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx      = Constraints.mustPayToPubKey (pPartyA channelParameters) (Ada.lovelaceValueOf cpBalanceA)  <>
                  Constraints.mustPayToPubKey (pPartyB channelParameters) (Ada.lovelaceValueOf cpBalanceB)  <>
                  Constraints.mustSpendScriptOutput oref r
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "closed channel %s with final balance (%d, %d)"
        (P.show channelParameters)
        cpBalanceA
        cpBalanceB

--
-- close logic
--

forceClose :: forall w s. ForceCloseParams -> Contract w s Text ()
forceClose ForceCloseParams{..} = do
    (oref, o, d@ChannelDatum{..}) <- findChannel fpChannelId
    logInfo @P.String $ printf "found channel utxo with datum %s" (P.show d)
    when (not disputed) $
        throwError $ pack $ printf "channel was never in disputed state"
    let r       = Redeemer $ PlutusTx.toBuiltinData ForceClose
        lookups = Constraints.typedValidatorLookups typedChannelValidator P.<>
                  Constraints.otherScript channelValidator                P.<>
                  Constraints.unspentOutputs (Map.singleton oref o)
        tx      = Constraints.mustPayToPubKey (pPartyA channelParameters) (Ada.lovelaceValueOf (balanceA state))  <>
                  Constraints.mustPayToPubKey (pPartyB channelParameters) (Ada.lovelaceValueOf (balanceB state))  <>
                  Constraints.mustValidateIn (from (time + 1 + (fromMilliSeconds (DiffMilliSeconds (pTimeLock channelParameters))))) <>
                  Constraints.mustSpendScriptOutput oref r
    ledgerTx <- submitTxConstraintsWith lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "force closed channel %s with final balance (%d, %d)"
        (P.show channelParameters)
        (balanceA state)
        (balanceB state)

findChannel :: Integer
            -> Contract w s Text (TxOutRef, ChainIndexTxOut, ChannelDatum)
findChannel cId = do
    utxos <- utxosAt $ scriptHashAddress channelHash
    let xs = [ (oref, o)
             | (oref, o) <- Map.toList utxos
             ]
    case xs of
        [(oref, o)] -> case _ciTxOutDatum o of
            Left _          -> throwError "datum missing"
            Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> throwError "datum has wrong type"
                Just d@ChannelDatum{..}
                    | pChannelId channelParameters == cId -> return (oref, o, d)
                    | otherwise                                           -> throwError "channel id missmatch"
        _           -> throwError "channel utxo not found"


--
-- Actual Endpoints for start, bid and close
--

endpoints :: Contract () ChannelSchema Text ()
endpoints = awaitPromise (open' `select` dispute' `select` close' `select` forceClose') >> endpoints
  where
    open' = endpoint @"open" open
    dispute'   = endpoint @"dispute"   dispute
    close' = endpoint @"close" close
    forceClose' = endpoint @"forceClose" forceClose

mkSchemaDefinitions ''ChannelSchema


mkKnownCurrencies []

