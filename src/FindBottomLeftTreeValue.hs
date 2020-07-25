module FindBottomLeftTreeValue where

import           ADT.Tree

-- | Given a binary tree, find the leftmost value in the last row of the tree. 
findBottomLeftValue :: Tree a -> a
findBottomLeftValue t = go [t]
 where
  go tss@(t : ts) | null $ concatMap children tss = val t
                  | otherwise                     = go $ concatMap children tss
