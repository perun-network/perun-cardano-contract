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

module Main where

import Data.Aeson
  ( FromJSON (..),
    Options (..),
    ToJSON (..),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import qualified Data.OpenApi as OpenApi
import GHC.Generics (Generic)
import Perun.Offchain as Perun
import Plutus.PAB.Effects.Contract.Builtin (SomeBuiltin (..))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Run (runWith)
import Prettyprinter (Pretty (..), viaShow)
import Prelude (Bool (..), Eq (..), IO, Ord (..), Show (..))

main :: IO ()
main = runWith (Builtin.handleBuiltin @StarterContracts)

data StarterContracts
  = PerunContract
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
  getContract = \case
    PerunContract -> SomeBuiltin Perun.contract
