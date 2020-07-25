module MaximumBinaryTree where

import           ADT.Tree

maxBinaryTree :: [Int] -> Tree Int
maxBinaryTree [] = Nil
maxBinaryTree xs =
  let (ls, max : rs) = span (/= maximum xs) xs
  in  Node (maxBinaryTree ls) max (maxBinaryTree rs)
