{-# LANGUAGE TemplateHaskell #-}

-- | Module describing the offchain errors the Perun-Contract
-- implementation can emit.
module Perun.Error
  ( PerunError (..),
    AsPerunError (..),
  )
where

import Control.Lens
import Plutus.Contract

data PerunError
  = -- | Generic ContractError, encapsulated here s.t. it can be caught
    -- together with Perun specific contract errors.
    PerunContractError !ContractError
  | -- | Thrown when a party has balances remaining within (0, minAda).
    InsufficientMinimumAdaFundingError
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
  | -- | Thrown when no UTXO is found, which contains a valid channel
    -- datum.
    FindChannelWrongDatumTypeError
  | -- | Thrown when no UTXO is found, which contains any datum.
    FindChannelDatumMissingError
  | -- | Thrown when no UTXOs are found.
    FindChannelNoUTXOsError
  | -- | Thrown when an unexpected number of UTXOs are found for a given
    -- channel id.
    -- TODO: This most likely has to be removed, since we cannot prevent
    -- anyone from just sending UTXOs to our channel.
    FindChannelUnexpectedNumberOfUTXOsError
  deriving (Eq, Show)

makeClassyPrisms ''PerunError

instance AsContractError PerunError where
  _ContractError = _PerunContractError
