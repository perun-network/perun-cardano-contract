module Perun.Adjudicator.History where

import Data.Aeson
import GHC.Generics
import Perun.Onchain

-- | ChannelHistory describes the on-chain history of a Perun channel.
data ChannelHistory
  = ChannelCreation !ChannelDatum !ChannelHistory
  | ChannelStep !ChannelAction !ChannelDatum !ChannelHistory
  | ChannelConclude !ChannelAction !ChannelHistory
  | ChannelNil
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
