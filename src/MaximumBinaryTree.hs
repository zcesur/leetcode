module MaximumBinaryTree where

import           ADT.Tree

maxBinaryTree :: [Int] -> Tree Int
maxBinaryTree [] = Nil
maxBinaryTree xs = Node (maxBinaryTree ls) (head rs) (maxBinaryTree (tail rs))
  where (ls, rs) = span (/= maximum xs) xs
