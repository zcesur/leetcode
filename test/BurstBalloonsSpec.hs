module BurstBalloonsSpec where

import BurstBalloons
import Test.Hspec

spec :: Spec
spec = do
  describe "maxCoins" $ do
    it "can find the maximum coins you can collect by bursting balloons" $ do
      maxCoins [3,1,5,8] `shouldBe` 167
