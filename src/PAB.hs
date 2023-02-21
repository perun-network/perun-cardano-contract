{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PAB
  ( StarterContracts (..),
  )
where

import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (..),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import Orphans ()
import qualified Data.OpenApi as OpenApi
import GHC.Generics (Generic)
import Perun.Adjudicator as Perun
import Perun.Offchain as Perun
import Perun.Onchain as Perun
import Plutus.PAB.Effects.Contract.Builtin (SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Prettyprinter (Pretty (..), viaShow)
import Prelude (Bool (..), Eq (..), Ord (..), Show (..))

data StarterContracts
  = PerunContract
  | AdjudicatorContract !Perun.ChannelID
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (OpenApi.ToSchema)

instance ToJSON StarterContracts where
  toJSON =
    genericToJSON
      defaultOptions
        { tagSingleConstructors = True
        }

instance FromJSON StarterContracts where
  parseJSON =
    genericParseJSON
      defaultOptions
        { tagSingleConstructors = True
        }

instance Pretty StarterContracts where
  pretty = viaShow

instance Builtin.HasDefinitions StarterContracts where
  getDefinitions = [PerunContract]
  getSchema = \case
    PerunContract -> Builtin.endpointsToSchemas @Perun.ChannelSchema
    AdjudicatorContract _ -> Builtin.endpointsToSchemas @Perun.AdjudicatorSchema
  getContract = \case
    PerunContract -> SomeBuiltin Perun.contract
    AdjudicatorContract cID -> SomeBuiltin (Perun.adjudicatorSubscription cID)
