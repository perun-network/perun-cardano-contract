{-# LANGUAGE TypeApplications #-}

module Main where

import PAB (StarterContracts)
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import Plutus.PAB.Run (runWith)

main :: IO ()
main = runWith (Builtin.handleBuiltin @StarterContracts)
