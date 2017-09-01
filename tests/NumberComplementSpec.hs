module NumberComplementSpec where

import Test.Hspec
import NumberComplement

spec :: Spec
spec = do
  describe "findComplement" $ do
    it "can find a complement of a number" $ do
      findComplement 5 `shouldBe` 2
      findComplement 1 `shouldBe` 0
