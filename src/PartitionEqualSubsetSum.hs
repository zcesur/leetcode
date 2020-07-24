module PartitionEqualSubsetSum where

-- | Given a non-empty array containing only positive integers, find if the
-- array can be partitioned into two subsets such that the sum of elements in
-- both subsets is equal.
canPartition :: [Int] -> Bool
canPartition xs
    | s `mod` 2 == 0 = isSubsetSum xs $ s `div` 2
    | otherwise           = False
    where s = sum xs

isSubsetSum :: [Int] -> Int -> Bool
isSubsetSum _ 0 = True
isSubsetSum [] _ = False
isSubsetSum (x:xs) n
    | n < 0     = False
    | otherwise = isSubsetSum xs (n-x) || isSubsetSum xs n
