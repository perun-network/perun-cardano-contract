{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Perun.Offchain.State where

import Data.Aeson
import Perun.Offchain.Event
import Perun.Onchain
import qualified Plutus.Script.Utils.V1.Typed.Scripts as Typed

newtype PerunState = PChannel ChannelDatum
  deriving (Show, ToJSON, FromJSON)

type PerunEvent = [ChannelEvent]

perunState :: OnChainState -> ChannelDatum
perunState (OnChainState ref) = Typed.tyTxOutData . Typed.tyTxOutRefOut $ ref

newtype OnChainState = OnChainState
  { ocsTxOutRef :: Typed.TypedScriptTxOutRef ChannelTypes
  }
