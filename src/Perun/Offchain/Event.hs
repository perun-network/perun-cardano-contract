module Perun.Offchain.Event where

import Data.Aeson
import GHC.Generics
import Perun.Onchain (ChannelDatum (..), version)

-- | ChannelEvent defines channel events which happen on-chain and can be
-- viewed off-chain.
data ChannelEvent
  = -- | Created event, emitted when the channel of interested was first
    -- deployed on-chain.
    Created !ChannelDatum
  | -- | Deposited event, emitting the funding index that deposited the given
    -- values described as a list of balances.
    Deposited !ChannelDatum !Integer ![Balance]
  | -- | Disputed event, emitting the claimed state on-chain.
    Disputed !ChannelDatum
  | -- | Progressed event, indicating the new version number for a channel
    -- state.
    Progressed !ChannelDatum !Integer
  | -- | Concluded event, emitted when a channel is concluded and closed.
    Concluded !ChannelDatum
  | -- | Withdrawn event, emitted when a channels funds are redistributed to
    -- its participants.
    Withdrawn
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Balance is a generic description for balances in Cardano. This can either
-- be the native currency ADA, or any other Token.
type Balance = Integer
