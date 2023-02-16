module Perun.Offchain.ChannelTxPool.Types where

import Control.Lens.TH
import Ledger (TxOutRef)
import Perun.Onchain
import Plutus.ChainIndex hiding (txFromTxId)
import Plutus.ChainIndex.Api

-- | ChannelTxPool is a pool of ChannelTxs for a specific channel id.
-- Following invariants hold:
--  1. All TXs contained are ONLY relevant for a single channel id.
newtype ChannelTxPool = ChannelTxPool_ [ChannelTx]

-- | ChannelTx is a ChainIndexTx associated with a ChannelID. Done by
-- construction.
data ChannelTx = ChannelTx_
  { -- | The ChainIndexTx of interest either consuming or producing a channel.
    _channelTxChannelcitx :: !ChainIndexTx,
    -- | The ChannelTxInRef, if the ChanIndexTx contains a channel UTXO in one
    -- of its inputs, together with the ChannelDatum and Redeemer action.
    _channelTxChannelTxInRef :: !(Maybe (TxOutRef, ChannelAction, ChannelDatum)),
    -- | The ChannelTxOutRef, if the ChainIndexTx contains a channel UTXO in
    -- its output.
    _channelTxChannelTxOutRef :: !(Maybe (TxOutRef, ChannelDatum))
  }
  deriving (Eq, Show)

-- Pattern synonyms to allow hiding the internal representation of ChannelTx
-- and ChannelTxPool. This ensures that we can use smart constructors to
-- enforce our invariants on our types, expose the Types without
-- type constructors (ChannelTxPool_, ChannelTx_) and STILL allow users to
-- use pattern matching to conveniently access subfields.
--
-- I.e. Users of the ChannelTxPool module will not be able to construct a
-- ChannelTx or ChannelTxPool using `ChannelTx_` and `ChannelTxPool_`
-- respectively. They instead have to use our exposed smart constructors
-- defined in `Perun.Offchain.ChannelTxPool.ChannelTxPool`.

pattern ChannelTxPool :: [ChannelTx] -> ChannelTxPool
pattern ChannelTxPool txs = ChannelTxPool_ txs

pattern ChannelTx ::
  ChainIndexTx ->
  Maybe (TxOutRef, ChannelAction, ChannelDatum) ->
  Maybe (TxOutRef, ChannelDatum) ->
  ChannelTx
pattern ChannelTx citx txinref txoutref = ChannelTx_ citx txinref txoutref

makeFields ''ChannelTxPool
