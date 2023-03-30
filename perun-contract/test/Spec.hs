-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module Main (main) where

import Perun.TestCases
import Test.Tasty

main :: IO ()
main = defaultMain perunTests
