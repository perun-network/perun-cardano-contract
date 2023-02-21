module Orphans where

import qualified Data.OpenApi as OpenApi
import qualified Perun.Onchain as Perun

instance OpenApi.ToSchema Perun.ChannelID
