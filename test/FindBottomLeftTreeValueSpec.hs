module FindBottomLeftTreeValueSpec where

import FindBottomLeftTreeValue
import ADT.Tree
import Test.Hspec

spec :: Spec
spec = do
  describe "findBottomLeftValue" $ do
    it "can find the leftmost value in the last row of the tree." $ do
      let t1 = Node t2 1 t3
          t2 = Node t4 2 Nil
          t3 = Node t5 3 t6
          t4 = Node Nil 4 Nil
          t5 = Node t7 5 Nil
          t6 = Node Nil 6 Nil
          t7 = Node Nil 7 Nil
      findBottomLeftValue t1 `shouldBe` 7
