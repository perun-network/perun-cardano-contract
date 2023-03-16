module Perun.Offchain.Event where

import Data.Aeson
import GHC.Generics
import Perun.Onchain (ChannelDatum)

-- | ChannelEvent defines channel events which happen on-chain and can be
-- viewed off-chain.
data ChannelEvent
  = -- | Created event, emitted when the channel of interested was first
    -- deployed on-chain.
    Created ![ChannelDatum]
  | -- | Deposited event, emitting the funding index that deposited the given
    -- values described as a list of balances.
    Deposited ![ChannelDatum]
  | -- | Disputed event, emitting the claimed state on-chain.
    Disputed ![ChannelDatum]
  | -- | Concluded event, emitted when a channel is concluded and closed.
    Concluded ![ChannelDatum]
  deriving (Show, Generic, ToJSON, FromJSON)

-- | Balance is a generic description for balances in Cardano. This can either
-- be the native currency ADA, or any other Token.
type Balance = Integer

-- | ChannelEventsLast is a list of channel events. We create this wrapper type
-- to define a custom Monoid instance for it for use in the AdjudicatorContract
-- state, in order to keep the JSON encoding as simple as possible.
newtype ChannelEventsLast = ChannelEventsLast [ChannelEvent]

instance Semigroup ChannelEventsLast where
  _ <> b = b

instance Monoid ChannelEventsLast where
  mempty = ChannelEventsLast []

instance ToJSON ChannelEventsLast where
  toJSON (ChannelEventsLast evs) = toJSON evs

instance FromJSON ChannelEventsLast where
  parseJSON = fmap ChannelEventsLast . parseJSON
