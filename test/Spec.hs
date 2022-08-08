-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module Main (main) where

import PerunPlutus.TestCases
import Test.Tasty

main :: IO ()
main = defaultMain perunTests
