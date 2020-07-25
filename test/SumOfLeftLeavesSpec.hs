module SumOfLeftLeavesSpec where

import           ADT.Tree
import           SumOfLeftLeaves
import           Test.Hspec

spec :: Spec
spec = do
  it "can find the sum of left leaves of a binary tree" $ do
    let t = Node (leaf 9) 3 (Node (leaf 15) 20 (leaf 7))
    sumOfLeftLeaves t `shouldBe` 24
