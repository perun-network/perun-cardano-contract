module Perun.Offchain.State where

import Data.Aeson
import GHC.Generics
import Perun.Offchain.Event
import Perun.Onchain
import qualified Plutus.Script.Utils.V2.Typed.Scripts as Scripts

newtype PerunState = PChannel ChannelDatum
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type PerunEvent = ChannelEventsLast

perunState :: OnChainState -> ChannelDatum
perunState (OnChainState ref) = Scripts.tyTxOutData . Scripts.tyTxOutRefOut $ ref

channelTokenFromOnChainState :: OnChainState -> ChannelToken
channelTokenFromOnChainState = channelToken . perunState

newtype OnChainState = OnChainState
  { ocsTxOutRef :: Scripts.TypedScriptTxOutRef ChannelTypes
  }
