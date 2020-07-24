module CountingBitsSpec where

import CountingBits
import Test.Hspec
import Util (toBinary)

spec :: Spec
spec = do
  describe "countBits" $ do
    it "can count bits" $ do
      countBits 100 `shouldBe` map (sum . toBinary) [0..100]
