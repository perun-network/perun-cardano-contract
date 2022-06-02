module PerunPlutus.PerunDummySpec where

import Test.Hspec
  ( Spec,
    describe,
    it,
  )
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec =
  describe "PerunPlutus" $
    it "does nothing yet" $ 1 `shouldBe` 1
