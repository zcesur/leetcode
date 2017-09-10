module MaximumBinaryTreeSpec where

import MaximumBinaryTree
import Test.Hspec
import ADT.Tree

spec :: Spec
spec = do
  describe "maxBinaryTree" $ do
    it "can construct the maximum tree by the given list" $ do
      let t6 = Node t3 6 t5
          t3 = Node Nil 3 t2
          t5 = Node t0 5 Nil
          t2 = Node Nil 2 t1
          t0 = Node Nil 0 Nil
          t1 = Node Nil 1 Nil

      maxBinaryTree [3,2,1,6,0,5] `shouldBe` t6
