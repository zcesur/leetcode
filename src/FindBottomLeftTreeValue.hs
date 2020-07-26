module FindBottomLeftTreeValue where

import           ADT.Tree

-- | Given a binary tree, find the leftmost value in the last row of the tree. 
findBottomLeftValue :: Tree a -> a
findBottomLeftValue t = go [t]
 where
  go ts | null $ concatMap children ts = val (head ts)
        | otherwise                    = go $ concatMap children ts
