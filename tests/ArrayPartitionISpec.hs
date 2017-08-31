module ArrayPartitionISpec where

import Test.Hspec
import ArrayPartitionI
import Data.List (permutations)

spec :: Spec
spec = do
  describe "splitAfterEvery" $ do
    it "split a list into sublists of size n (except for the last one)" $ do
      splitAfterEvery 3 [1..6] `shouldBe` [[1..3], [4..6]]
      splitAfterEvery 2 [1..5] `shouldBe` [[1,2], [3,4], [5]]

  describe "arrayPairSum" $ do
    it "can group the integers such that the desired sum is maximized" $ do
      arrayPairSum [1,4,3,2] `shouldBe` 4
