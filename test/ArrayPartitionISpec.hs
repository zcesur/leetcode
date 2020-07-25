module ArrayPartitionISpec where

import           Test.Hspec
import           ArrayPartitionI

spec :: Spec
spec = do
  describe "arrayPairSum" $ do
    it "can group the integers such that the desired sum is maximized" $ do
      arrayPairSum [1, 4, 3, 2] `shouldBe` 4
