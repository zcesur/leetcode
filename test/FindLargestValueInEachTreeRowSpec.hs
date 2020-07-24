module FindLargestValueInEachTreeRowSpec where

import FindLargestValueInEachTreeRow
import Test.Hspec
import ADT.Tree

spec :: Spec
spec = do
  describe "largestValues" $ do
    it "can find the largest value in each tree row" $ do
      let tree = Node (Node (leaf 5) 3 (leaf 3)) 1 (Node Nil 2 (leaf 9))

      largestValues tree `shouldBe` [1,3,9]
