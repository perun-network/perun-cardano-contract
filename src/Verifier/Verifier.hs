{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}


module Verifier.Verifier where

import Ledger.Constraints ( TxConstraints, ScriptLookups )
import Types

data ChanSchema = OpenType | DisputeType | CloseType | ForceCloseType

type family VerifierParams a 

class Verifier m params | m -> params  where
    verifyOpen :: params -> m params (ScriptLookups ChannelTypes, TxConstraints ChannelAction ChannelDatum)
    verifyDispute :: params -> m params (ScriptLookups ChannelTypes, TxConstraints ChannelAction ChannelDatum)
    verifyClose :: params -> m params (ScriptLookups ChannelTypes, TxConstraints ChannelAction ChannelDatum)
    verifyForceClose :: params -> m params (ScriptLookups ChannelTypes, TxConstraints ChannelAction ChannelDatum)