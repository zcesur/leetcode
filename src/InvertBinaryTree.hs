module InvertBinaryTree where

import           ADT.Tree

-- | Invert a binary tree.
invertTree :: Tree a -> Tree a
invertTree Nil          = Nil
invertTree (Node l x r) = Node (invertTree r) x (invertTree l)
