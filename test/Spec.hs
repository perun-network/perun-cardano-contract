-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module Main(main) where

import PerunPlutus.PerunDummySpec (propUnitTest)
import Test.QuickCheck

main :: IO ()
main = quickCheck propUnitTest
