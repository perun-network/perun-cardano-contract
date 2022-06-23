-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module Main(main) where

import PerunPlutus.TestCases (propUnitTest)
import Test.QuickCheck

main :: IO ()
main =  do
    quickCheck propUnitTest
