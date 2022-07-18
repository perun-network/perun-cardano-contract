-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module Main (main) where

import Control.Monad (void)
import PerunPlutus.TestCases

main :: IO ()
main = void runPerunTests
