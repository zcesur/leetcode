module ADT.BST where

import           ADT.Tree

insert :: Ord a => Tree a -> a -> Tree a
insert Nil            x = leaf x
insert t@(Node l v r) x = case x `compare` v of
  EQ -> t
  LT -> Node (insert l x) v r
  GT -> Node l v (insert r x)

rotR :: Tree a -> Tree a
rotR Nil                       = undefined
rotR (Node Nil           _  _) = undefined
rotR (Node (Node a xv b) yv c) = Node a xv (Node b yv c)

rotL :: Tree a -> Tree a
rotL Nil                       = undefined
rotL (Node _ _  Nil          ) = undefined
rotL (Node a xv (Node b yv c)) = Node (Node a xv b) yv c
