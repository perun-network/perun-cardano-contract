-- | Module describing the offchain errors the Perun-Contract
-- implementation can emit.
module Perun.Error
  ( PerunError (..),
    FindChannelException (..),
    SubscriptionException (..),
    DatumException (..),
    ChannelTxErr (..),
    AsPerunError (..),
  )
where

import Control.Lens
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger.Tx (ToCardanoError)
import Perun.Adjudicator.History
import Plutus.Contract

data PerunError
  = -- | Generic ContractError, encapsulated here s.t. it can be caught
    -- together with Perun specific contract errors.
    PerunContractError !ContractError
  | -- | Thrown when a party has balances remaining within (0, minAda).
    InsufficientMinimumAdaBalanceError
  | -- | Thrown when an already funded channel is funded again.
    RedundantFundError
  | -- | Thrown when trying to abort an already funded channel.
    AlreadyFundedAbortError
  | -- | Thrown when an invalid state transition is detected. This
    -- includes:
    -- * ChannelID changes between states.
    -- * Sum of all locked balances changes.
    -- * New version of the channel is not strictly greater than before.
    -- * Trying to transition out of the terminal (final) state.
    InvalidStateTransitionError
  | -- | Thrown when a dispute is called on a channel that is not already
    -- funded.
    PrematureDisputeError
  | -- | Thrown when close on a channel is called, which was not already
    -- finalized.
    CloseOnNonFinalChannelError
  | -- | Thrown when close on a channel is called, which was not already
    -- funded. Closing a non-funded channel requires a call to `abort`.
    CloseOnNonFundedChannelError
  | -- | Thrown when force close on a channel is called, which is not
    -- already in disputed state.
    ForceCloseOnNonDisputedChannelError
  | -- | Thrown when force close on a channel is called, which was not
    -- already funded.
    ForceCloseOnNonFundedChannelError
  | -- | Thrown when the channel id given to an endpoint does not align
    -- with the channel id in a signed state or the computed channel id
    -- from the channel parameters.
    ChannelIDMismatchError
  | FindChannelError !FindChannelException
  | SubscriptionError !SubscriptionException
  | SomeContractError !ContractError
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data FindChannelException
  = -- | Thrown when no UTXO is found, which contains a valid channel
    -- datum.
    WrongDatumTypeError
  | --  | Thrown when no UTXO is found, which contains any datum.
    DatumMissingError
  | -- | Thrown when no UTXOs are found.
    NoUTXOsError
  | -- | Thrown when an unexpected number of UTXOs are found for a given
    -- channel id.
    UnexpectedNumberOfUTXOsError
  | -- | Thrown when no UTXO is found, which contains the ChannelToken
    -- as value
    NoThreadTokenError
  | -- | Thrown when the channel datum attached to the channel UTXO does
    -- not pass validation.
    InvalidDatumError
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SubscriptionException
  = InvalidOnchainStatesForChooserErr !Text
  | UnexpectedNumberOfChannelUTXOsErr
  | DatumErr !DatumException
  | CorruptedChainIndexErr
  | NoTxFromTxIdFetchableErr
  | NoChannelTokenAvailableErr
  | MismatchedChannelTokenErr
  | ImpossibleChannelHistory !ChannelHistory
  | ChannelErr !ChannelTxErr
  | DivergentChannelHistory
  | MultipleChannelHistoryErr
  | MultipleChannelStartsErr
  | NoOnchainStatesFoundErr
  | ParsingChannelActionErr
  | ContractEndedErr
  | NoInputMatchingOnChainStateRefErr
  | NoInputMatchingRefErr
  | InputNotContainingChannelActionErr
  | ParsingChannelActionRedeemerErr
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DatumException
  = NoOutputDatumErr
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ChannelTxErr
  = ChannelTxErr
  | NoChannelOutputDatumErr
  | NoChannelInputRedeemerErr
  | NoChannelInputRefErr
  | NoRedeemerForInputErr
  | NoDatumForInputErr
  | NoOutputMatchinInputErr
  | NoChannelInputErr
  | NoStartStateFoundErr
  | InvalidTxErr
  | NoChannelOutputErr
  | UnexpectedInvalidTxErr
  | ChannelCorruptedChainIndexErr
  | WrongThreadTokenErr
  | InvalidChannelDatumErr
  | SomeToCardanoErr !ToCardanoError
  | InvalidTxOutRefErr
  | InvalidThreadTokenErr
  | PerunAsReferenceUnsupportedErr
  | InlineDatumsUnsupportedErr
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''PerunError

instance AsContractError PerunError where
  _ContractError = _SomeContractError
