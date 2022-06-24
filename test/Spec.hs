-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module Main (main) where

import PerunPlutus.TestCases
import Control.Monad (void)

main :: IO ()
main = void runPerunDummyTests
