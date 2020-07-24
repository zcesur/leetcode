module InvertBinaryTreeSpec where

import InvertBinaryTree
import Test.Hspec
import ADT.Tree

spec :: Spec
spec = do
  it "can invert a tree" $ do
    let t  = Node (Node (leaf 1) 2 (leaf 3)) 4 (Node (leaf 6) 7 (leaf 9))
        t' = Node (Node (leaf 9) 7 (leaf 6)) 4 (Node (leaf 3) 2 (leaf 1)) 

    invertTree t `shouldBe` t'
