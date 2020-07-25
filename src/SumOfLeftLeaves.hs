module SumOfLeftLeaves where

import           ADT.Tree

-- | Find the sum of all left leaves in a given binary tree.
sumOfLeftLeaves :: (Num a) => Tree a -> a
sumOfLeftLeaves Nil          = 0
sumOfLeftLeaves (Node l _ r) = sumOfLeftLeaves r + case l of
  Node Nil _ Nil -> val l
  _              -> sumOfLeftLeaves l
