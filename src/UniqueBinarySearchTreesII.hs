module UniqueBinarySearchTreesII where

import ADT.Tree
import ADT.BST (insert, rotR, rotL)

-- | Given an integer n, generate all structurally unique BST's (binary
-- search trees) that store values 1...n.
generateTrees :: Int -> [Tree Int]
generateTrees n = generateTreesHelper $ foldl insert Nil [1..n]

-- | Helper function that generates all structurally unique BST's given a BST
-- where for all nodes, either left or right child is an empty tree.
generateTreesHelper :: Ord a => Tree a -> [Tree a]
generateTreesHelper Nil = [Nil]
generateTreesHelper t@(Node l x r) = case (l, r) of
    (Nil, Nil) -> [t]
    (Nil, _)   -> concatMap go $ map ($ t) $ selfCompose rotL (height t)
    (_, Nil)   -> concatMap go $ map ($ t) $ selfCompose rotR (height t)
    (_, _)     -> undefined
  where
    -- Self compose a function up to n times and store the results in a list
    selfCompose :: (a -> a) -> Int -> [a -> a]
    selfCompose f n = scanl (.) id (replicate n f)
    
    -- Generate all structurally unique BST's where the root node of the given
    -- BST stays fixed.
    go :: Ord a => Tree a -> [Tree a]
    go (Node l x r) = [Node l' x r' | l' <- generateTreesHelper l
                                    , r' <- generateTreesHelper r]
