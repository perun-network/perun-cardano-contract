{-# LANGUAGE FunctionalDependencies #-}

module Perun.Offchain.ChannelTxPool.Types where

import Control.Lens.TH
import Ledger (TxOutRef)
import Perun.Onchain
import Plutus.ChainIndex hiding (txFromTxId)

-- | ChannelTxPool is a pool of ChannelTxs for a specific channel id identified
-- by a corresponding ChannelToken.
data ChannelTxPool = ChannelTxPool_ ChannelToken [ChannelTx]

-- | ChannelTx is a ChannelTx without any constraints on the input and output.
type ChannelTx =
  ChannelTxDynamic
    (Maybe (TxOutRef, ChannelAction, ChannelDatum))
    (Maybe (TxOutRef, ChannelDatum))

-- | ChannelTxStep is a ChannelTx which was proven to be a step in a channels
-- lifecycle.
type ChannelTxStep =
  ChannelTxDynamic
    (TxOutRef, ChannelAction, ChannelDatum)
    (TxOutRef, ChannelDatum)

-- | ChannelTxLast is a ChannelTx which was proven to be the last step in a
-- channels lifecycle so far.
type ChannelTxLast =
  ChannelTxDynamic
    (TxOutRef, ChannelAction, ChannelDatum)
    (Maybe (TxOutRef, ChannelDatum))

-- | ChannelTxFirst is a ChannelTx which was proven to be the first step in a
-- channels lifecycle.
type ChannelTxFirst =
  ChannelTxDynamic
    (Maybe (TxOutRef, ChannelAction, ChannelDatum))
    (TxOutRef, ChannelDatum)

-- | ChannelTx is a ChainIndexTx associated with a ChannelID. Done by
-- construction.
data ChannelTxDynamic i o = ChannelTx_
  { -- | The ChainIndexTx of interest either consuming or producing a channel.
    _channelTxDynamicChannelcitx :: !ChainIndexTx,
    -- | The ChannelTxInRef, if the ChanIndexTx contains a channel UTXO in one
    -- of its inputs, together with the ChannelDatum and Redeemer action.
    _channelTxDynamicChannelTxInRef :: !i,
    -- | The ChannelTxOutRef, if the ChainIndexTx contains a channel UTXO in
    -- its output.
    _channelTxDynamicChannelTxOutRef :: !o
  }
  deriving (Show)

-- Pattern synonyms allow hiding the internal representation of ChannelTx
-- and ChannelTxPool. This ensures that we can use smart constructors to
-- enforce our invariants on our types, expose the Types without
-- type constructors (ChannelTxPool_, ChannelTx_) and STILL allow users to
-- use pattern matching to conveniently access subfields.
--
-- I.e. Users of the ChannelTxPool module will not be able to construct a
-- ChannelTx or ChannelTxPool using `ChannelTx_` and `ChannelTxPool_`
-- respectively. They instead have to use our exposed smart constructors
-- defined in `Perun.Offchain.ChannelTxPool.ChannelTxPool`.

pattern ChannelTxPool :: ChannelToken -> [ChannelTx] -> ChannelTxPool
pattern ChannelTxPool ct txs = ChannelTxPool_ ct txs

{-# COMPLETE ChannelTxPool #-}

pattern ChannelTx :: ChainIndexTx -> i -> o -> ChannelTxDynamic i o
pattern ChannelTx citx txinref txoutref = ChannelTx_ citx txinref txoutref

{-# COMPLETE ChannelTx #-}

generalizeFirst :: ChannelTxFirst -> ChannelTx
generalizeFirst (ChannelTx_ citx txinref txoutref) =
  ChannelTx_ citx txinref (Just txoutref)

generalizeStep :: ChannelTxStep -> ChannelTx
generalizeStep (ChannelTx_ citx txinref txoutref) =
  ChannelTx_ citx (Just txinref) (Just txoutref)

generalizeLast :: ChannelTxLast -> ChannelTx
generalizeLast (ChannelTx_ citx txinref txoutref) =
  ChannelTx_ citx (Just txinref) txoutref

makeFields ''ChannelTxDynamic
