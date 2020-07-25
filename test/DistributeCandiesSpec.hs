module DistributeCandiesSpec where

import           Test.Hspec
import           DistributeCandies

spec :: Spec
spec = do
  describe "distributeCandies" $ do
    it "can distribute the candies with the given conditions" $ do
      distributeCandies [1, 1, 2, 2, 3, 3] `shouldBe` 3
      distributeCandies [1, 1, 2, 3] `shouldBe` 2
