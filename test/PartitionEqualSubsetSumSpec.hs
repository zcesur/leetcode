module PartitionEqualSubsetSumSpec where

import           PartitionEqualSubsetSum
import           Test.Hspec

spec :: Spec
spec = do
  describe "canPartition" $ do
    it "can find if the array can be partitioned into two subsets" $ do
      canPartition [1, 5, 11, 5] `shouldBe` True
      canPartition [1, 2, 3, 5] `shouldBe` False
