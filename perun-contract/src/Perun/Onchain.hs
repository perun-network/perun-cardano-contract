{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Perun.Onchain
  ( Channel (..),
    ChannelState (..),
    SignedState (..),
    ChannelAction (..),
    ChannelDatum (..),
    ChannelTypes,
    ChannelID (..),
    ChannelToken (..),
    Action (..),
    minAda,
    isLegalOutValue,
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
    channelTokenAsset,
    channelTokenSymbol,
    channelTokenName,
    channelTokenValue,
    channelTokenValue',
    channelTokenValue'',
    mkChannelTokenMintingPolicy,
    mkVersionedChannelTokenMintinPolicy,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data
import Data.Text (unpack)
import GHC.Generics (Generic)
import Ledger
  ( Address,
    CurrencySymbol,
    DiffMilliSeconds (..),
    Extended (..),
    Interval (..),
    LowerBound (..),
    POSIXTime,
    PaymentPubKey (..),
    PaymentPubKeyHash (..),
    Signature,
    UpperBound (..),
    contains,
    from,
    fromMilliSeconds,
    member,
    minAdaTxOut,
    strictLowerBound,
    strictUpperBound,
    toPubKeyHash,
  )
import Ledger.Ada as Ada
import qualified Ledger.Constraints as Constraints
import Ledger.Scripts hiding (version)
import Ledger.Scripts.Orphans ()
import Ledger.Value (AssetClass (..), TokenName (..), assetClass, assetClassValueOf, geq)
import qualified Ledger.Value as Value
import Playground.Contract (ensureKnownCurrencies, printJson, printSchemas, stage)
import Plutus.Contract.Oracle (SignedMessage (..), verifySignedMessageConstraints)
import Plutus.Script.Utils.V2.Address (mkValidatorAddress)
import qualified Plutus.Script.Utils.V2.Scripts as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts hiding (validatorHash)
import Plutus.V2.Ledger.Contexts
  ( ScriptContext (..),
    TxInInfo (..),
    TxInfo (..),
    TxOut (..),
    TxOutRef (..),
    findDatum,
    getContinuingOutputs,
    ownCurrencySymbol,
    spendsOutput,
    txSignedBy,
  )
import Plutus.V2.Ledger.Tx
  ( OutputDatum (..),
    TxId (..),
  )
import qualified PlutusTx
import PlutusTx.Prelude hiding (unless)
import Schema (ToSchema (..))
import Text.Hex (encodeHex)
import qualified Prelude as P

--
--
-- ON CHAIN PART
--
--

minAda :: Integer
minAda = getLovelace minAdaTxOut

newtype ChannelID = ChannelID BuiltinByteString
  deriving (ToJSON, FromJSON, ToSchema, Eq, P.Eq, P.Ord) via BuiltinByteString
  deriving (Generic, Data)

PlutusTx.unstableMakeIsData ''ChannelID
PlutusTx.makeLift ''ChannelID

instance P.Show ChannelID where
  show (ChannelID cid) = unpack . encodeHex $ fromBuiltin cid

data ChannelToken = ChannelToken
  { ctSymbol :: CurrencySymbol,
    ctName :: TokenName,
    ctTxOutRef :: !TxOutRef
  }
  deriving (Data, Generic, ToJSON, FromJSON, ToSchema, P.Eq, P.Show)

instance P.Ord ChannelToken where
  {-# INLINEABLE compare #-}
  compare (ChannelToken a _ _) (ChannelToken b _ _) = compare a b

deriving instance Data TxId

deriving instance Data TxOutRef

instance Eq ChannelToken where
  {-# INLINEABLE (==) #-}
  a == b =
    ctSymbol a == ctSymbol b
      && ctName a == ctName b
      && ctTxOutRef a == ctTxOutRef b

PlutusTx.unstableMakeIsData ''ChannelToken
PlutusTx.makeLift ''ChannelToken

{-# INLINEABLE channelTokenAsset #-}
channelTokenAsset :: ChannelToken -> AssetClass
channelTokenAsset ct = assetClass (ctSymbol ct) (ctName ct)

{-# INLINEABLE channelTokenValue #-}
channelTokenValue :: CurrencySymbol -> TokenName -> Value.Value
channelTokenValue symbol name = Value.singleton symbol name 1

{-# INLINEABLE channelTokenValue' #-}
channelTokenValue' :: ChannelToken -> Value.Value
channelTokenValue' (ChannelToken s t _) = channelTokenValue s t

{-# INLINEABLE channelTokenValue'' #-}
channelTokenValue'' :: AssetClass -> Value.Value
channelTokenValue'' (AssetClass (symbol, name)) = channelTokenValue symbol name

-- Parameters of the channel
data Channel = Channel
  { pTimeLock :: !Integer,
    pSigningPKs :: ![PaymentPubKey],
    pPaymentPKs :: ![PaymentPubKeyHash],
    pNonce :: BuiltinByteString
  }
  deriving (P.Eq, P.Show, Generic, ToJSON, FromJSON)

-- Equality of two Channels
instance Eq Channel where
  {-# INLINEABLE (==) #-}
  a == b =
    pTimeLock a == pTimeLock b
      && pSigningPKs a == pSigningPKs b
      && pPaymentPKs a == pPaymentPKs b
      && pNonce a == pNonce b

PlutusTx.unstableMakeIsData ''Channel
PlutusTx.makeLift ''Channel

data ChannelState = ChannelState
  { channelId :: ChannelID,
    balances :: ![Integer],
    version :: !Integer,
    final :: !Bool
  }
  deriving (Data, Generic, ToJSON, FromJSON, ToSchema)
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

data SignedState = SignedState
  { sigs :: [Signature],
    datumHash :: DatumHash,
    channelState :: !ChannelState
  }
  deriving (Generic, ToJSON, FromJSON)
  deriving stock (P.Eq, P.Show)

instance Eq SignedState where
  {-# INLINEABLE (==) #-}
  a == b =
    sigs a == sigs b
      && channelState a == channelState b

-- TODO: datumHash a == datumHash b

instance ToSchema (SignedMessage ChannelState) where
  toSchema = toSchema @(Signature, (DatumHash, ChannelState))

PlutusTx.unstableMakeIsData ''SignedState
PlutusTx.makeLift ''SignedState

-- Redeemer Datatype
data ChannelAction = Fund | Abort | MkDispute !SignedState | MkClose !SignedState | ForceClose
  deriving (P.Eq, P.Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''ChannelAction
PlutusTx.makeLift ''ChannelAction

-- Datum datatype
data ChannelDatum = ChannelDatum
  { channelParameters :: !Channel,
    channelToken :: !ChannelToken,
    state :: !ChannelState,
    time :: !POSIXTime,
    funding :: ![Integer],
    funded :: !Bool,
    disputed :: !Bool
  }
  deriving (P.Eq, P.Show, Generic, ToJSON, FromJSON)

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

-- | isLegalOutValue returns true, iff the given value is either 0 or at least minAda
{-# INLINEABLE isLegalOutValue #-}
isLegalOutValue :: Integer -> Bool
isLegalOutValue v = v == 0 || v >= minAda

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

-- | Given a signed state and a list of public keys this returns the channel state `s`,
-- iff the signed state consists of exactly one valid signature on `s` for every given
-- public key in the correct order. It throws an error otherwise.
{-# INLINEABLE extractVerifiedState #-}
extractVerifiedState :: SignedState -> [PaymentPubKey] -> ChannelState
extractVerifiedState (SignedState sigs dh cs) signingKeys =
  let states =
        zipWithEqualLength
          verifySignedMessageConstraints'
          signingKeys
          (map (\sig -> SignedMessage sig dh (Datum $ PlutusTx.toBuiltinData cs)) sigs)
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
          -- check that every funding value is greater or equal to the minAda requirement
          traceIfFalse "funding value of less than minimum Ada" (all isLegalOutValue (funding outputDatum)),
          -- check that parameters, state, time stay the same, disputed == false
          traceIfFalse "violated channel integrity" channelIntegrityAtFunding,
          -- check that the channel is marked as funded iff it is actually funded
          -- the channel is funded iff the funding reflects the state in the output datum
          traceIfFalse "funded flag incorrect in output datum" checkFundingStatus,
          -- check that the integrity of the threadToken field in the ChannelDatum is not violated
          traceIfFalse "invalid threadToken field in output datum" keepsThreadTokenField,
          -- check that the channel output keeps the thread token
          traceIfFalse "channel output does own threadToken" keepsThreadToken
        ]
    Abort ->
      and
        [ -- no aborts on funded channels
          traceIfFalse "channel is already funded" (not $ funded oldDatum),
          -- check the authenticity of the abort to prevent DOS
          traceIfFalse "abort must be issued by channel participant" (any (txSignedBy info . unPaymentPubKeyHash) (pPaymentPKs $ channelParameters oldDatum)),
          traceIfFalse "wrong input funding" correctInputFunding,
          -- check that every party gets their funding refunded
          traceIfFalse "a party was not reimbursed correctly for their funding" (all (== True) (zipWith getsValue (pPaymentPKs (channelParameters oldDatum)) (map (payoutForPk (funding oldDatum)) (pPaymentPKs (channelParameters oldDatum))))),
          -- check that the thread token is burned
          traceIfFalse "the thread token is not burned on abort" burnsThreadToken
        ]
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
              -- check that the channel value is maintained. This also checks the integrity of the channel script
              traceIfFalse "wrong output value" correctChannelValue,
              -- check that the channel parameters stay the same
              traceIfFalse "channel parameters differ" (channelParameters oldDatum == channelParameters outputDatum),
              -- check that the time in the output datum is set properly
              traceIfFalse "invalid time in output datum" (allowedValidRangeSize && outputTimeInValidRange),
              -- check that the channel is marked as disputed
              traceIfFalse "failed to mark channel as disputed" (disputed outputDatum),
              -- check that the integrity of the threadToken field in the ChannelDatum is not violated
              traceIfFalse "invalid threadToken field in output datum" keepsThreadTokenField,
              -- check that the channel output keeps the thread token
              traceIfFalse "channel output does own threadToken" keepsThreadToken
            ]
    -- Close Case:
    MkClose st ->
      and
        [ traceIfFalse "wrong input value" correctInputValue,
          -- check that the close was issued on a funded state
          traceIfFalse "old state must be funded" (funded oldDatum),
          -- check that the thread token is burned
          traceIfFalse "the thread token is not burned on close" burnsThreadToken,
          let newState = extractVerifiedState st (pSigningPKs $ channelParameters oldDatum)
           in and
                [ traceIfFalse "Closing state does not belong to this channel" (cID == channelId newState),
                  -- check that the state is final
                  traceIfFalse "The closing state is not final" (final newState),
                  -- check that A receives their balance
                  traceIfFalse
                    "a party did not get their balance"
                    (all (== True) (zipWith getsValue (pPaymentPKs (channelParameters oldDatum)) (map (payoutForPk (balances newState)) (pPaymentPKs (channelParameters oldDatum)))))
                ]
        ]
    -- ForceClose Case:
    ForceClose ->
      and
        [ traceIfFalse "wrong input value" correctInputValue,
          -- check that the force close was issued on a funded state
          traceIfFalse "old state must be funded" (funded oldDatum),
          -- check that there was a prior dispute
          traceIfFalse "try to force close without prior dispute" (disputed oldDatum),
          -- check that the relative time-lock is past
          traceIfFalse "too early" correctForceCloseSlotRange,
          -- check that the thread token is burned
          traceIfFalse "the thread token is not burned on force close" burnsThreadToken,
          -- check that A receives their balance
          traceIfFalse
            "a party did not get their balance"
            (all (== True) (zipWith getsValue (pPaymentPKs (channelParameters oldDatum)) (map (payoutForPk (balances oldState)) (pPaymentPKs (channelParameters oldDatum)))))
        ]
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    input :: TxInInfo
    input =
      let isScriptInput i = case (txOutDatum . txInInfoResolved) i of
            NoOutputDatum -> False
            _otherwise -> True
          xs = [i | i <- txInfoInputs info, isScriptInput i]
       in case xs of
            [i] -> i
            _otherwise -> traceError "expected exactly one script input"

    inVal :: Value.Value
    inVal = txOutValue . txInInfoResolved $ input

    oldState :: ChannelState
    oldState = state oldDatum

    correctInputValue :: Bool
    correctInputValue = inVal == (Ada.lovelaceValueOf (sum $ balances oldState) <> channelTokenValue' (channelToken oldDatum))

    correctInputFunding :: Bool
    correctInputFunding = inVal == (Ada.lovelaceValueOf (sum $ funding oldDatum) <> channelTokenValue' (channelToken oldDatum))

    resolveDatumTarget :: TxOut -> BuiltinData -> (TxOut, ChannelDatum)
    resolveDatumTarget o d = case PlutusTx.fromBuiltinData d of
      Just ad' -> (o, ad')
      Nothing -> traceError "error decoding data"

    ownOutput :: TxOut
    outputDatum :: ChannelDatum
    (ownOutput, outputDatum) = case getContinuingOutputs ctx of
      [o] -> case txOutDatum o of
        NoOutputDatum -> traceError "wrong output type"
        OutputDatum (Datum d) -> resolveDatumTarget o d
        OutputDatumHash h -> case findDatum h info of
          Nothing -> traceError "datum not found"
          Just (Datum d) -> resolveDatumTarget o d
      _otherwise -> traceError "expected exactly one continuing output"

    channelIntegrityAtFunding :: Bool
    channelIntegrityAtFunding =
      channelParameters oldDatum == channelParameters outputDatum
        && state oldDatum == state outputDatum
        && time oldDatum == time outputDatum
        && not (disputed outputDatum)

    correctChannelValue :: Bool
    correctChannelValue =
      txOutValue ownOutput == (Ada.lovelaceValueOf (sum $ balances oldState) <> channelTokenValue' (channelToken oldDatum))

    correctChannelFunding :: Bool
    correctChannelFunding =
      txOutValue ownOutput == (Ada.lovelaceValueOf (sum $ funding outputDatum) <> channelTokenValue' (channelToken oldDatum))

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

    -- payoutForPk returns the sum of all the balances that belong to pk
    -- in a given balance distribution for this channel
    payoutForPk :: [Integer] -> PaymentPubKeyHash -> Integer
    payoutForPk bals pk =
      foldl
        ( \x element -> if fst element == pk then x + snd element else x
        )
        0
        (zip (pPaymentPKs $ channelParameters oldDatum) bals)

    -- getsValue returns true iff the funds in the output of this transaction
    -- belonging to pkh sum up to at least v
    getsValue :: PaymentPubKeyHash -> Integer -> Bool
    getsValue pkh v =
      (v == 0)
        || ( let outputsForParty =
                   [ o | o <- txInfoOutputs info, toPubKeyHash (txOutAddress o) == Just (unPaymentPubKeyHash pkh)
                   ]
              in sum (map txOutValue outputsForParty) `geq` Ada.lovelaceValueOf v
           )

    -- hasThreadToken :: TxOut -> Bool
    -- hasThreadToken o = case assetClassValueOf (txOutValue o) (threadToken oldDatum) of
    --  0 -> False
    --  1 -> True
    --  _else -> traceError "more than one ThreadToken in an output"

    hasThreadToken :: TxOut -> Bool
    hasThreadToken o = assetClassValueOf (txOutValue o) (channelTokenAsset (channelToken oldDatum)) == 1

    keepsThreadTokenField :: Bool
    keepsThreadTokenField = channelToken outputDatum == channelToken oldDatum

    keepsThreadToken :: Bool
    keepsThreadToken = hasThreadToken ownOutput

    burnsThreadToken :: Bool
    burnsThreadToken = all (not . hasThreadToken) (txInfoOutputs info)

--
-- COMPILATION TO PLUTUS CORE
--
--

typedChannelValidator :: ChannelID -> Scripts.TypedValidator ChannelTypes
typedChannelValidator =
  Scripts.mkTypedValidatorParam @ChannelTypes @ChannelID
    $$(PlutusTx.compile [||mkChannelValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.mkUntypedValidator @ChannelDatum @ChannelAction

channelValidator :: ChannelID -> Validator
channelValidator = Scripts.validatorScript . typedChannelValidator

channelHash :: ChannelID -> ValidatorHash
channelHash = Scripts.validatorHash . channelValidator

channelAddress :: ChannelID -> Address
channelAddress = mkValidatorAddress . channelValidator

data Action = Mint | Burn
  deriving (P.Eq, P.Show)

instance Eq Action where
  {-# INLINEABLE (==) #-}
  Mint == Mint = True
  Burn == Burn = True
  _ == _ = False

PlutusTx.makeIsDataIndexed ''Action [('Mint, 0), ('Burn, 1)]
PlutusTx.makeLift ''Action

{-# INLINEABLE mkChannelTokenPolicy #-}
mkChannelTokenPolicy :: TxOutRef -> (ValidatorHash, Action) -> ScriptContext -> Bool
mkChannelTokenPolicy (TxOutRef refHash refIdx) (vHash, action) ctx@ScriptContext {scriptContextTxInfo = txinfo} =
  let ownSymbol = ownCurrencySymbol ctx

      minted = txInfoMint txinfo
      expected = if action == Burn then -1 else 1

      -- True if the pending transaction mints the amount of
      -- currency that we expect
      mintOK =
        let v = checkThreadTokenInner ownSymbol vHash minted expected
         in traceIfFalse "S7" {-"Value minted different from expected"-} v

      -- True if the pending transaction spends the output
      -- identified by @(refHash, refIdx)@
      txOutputSpent =
        let v = spendsOutput txinfo refHash refIdx
         in traceIfFalse "S8" {-"Pending transaction does not spend the designated transaction output"-} v
   in mintOK && (if action == Mint then txOutputSpent else True)

mkChannelTokenMintingPolicy :: TxOutRef -> MintingPolicy
mkChannelTokenMintingPolicy oref =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||wrap . mkChannelTokenPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode oref
  where
    wrap = Scripts.mkUntypedMintingPolicy @(ValidatorHash, Action)

mkVersionedChannelTokenMintinPolicy :: TxOutRef -> Versioned MintingPolicy
mkVersionedChannelTokenMintinPolicy oref = Versioned (mkChannelTokenMintingPolicy oref) PlutusV2

channelTokenSymbol :: TxOutRef -> CurrencySymbol
channelTokenSymbol oref = scriptCurrencySymbol $ mkVersionedChannelTokenMintinPolicy oref

channelTokenName :: ValidatorHash -> TokenName
channelTokenName (ValidatorHash h) = TokenName h

{-# INLINEABLE checkThreadTokenInner #-}
checkThreadTokenInner :: CurrencySymbol -> ValidatorHash -> Value.Value -> Integer -> Bool
checkThreadTokenInner symbol (ValidatorHash vHash) vl i =
  Value.valueOf vl symbol (TokenName vHash) == i
