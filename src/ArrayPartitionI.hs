module ArrayPartitionI where

import Data.List (transpose, sort, permutations)
import Util (groupsOf)

-- | Given an array of 2n integers, group these integers into n pairs of
-- integer, say (a1, b1), (a2, b2), ..., (an, bn) which makes sum of min(ai,
-- bi) for all i from 1 to n as large as possible. 
arrayPairSum :: [Int] -> Int
arrayPairSum [] = 0
arrayPairSum xs = sum . head . transpose . (groupsOf 2) . sort $ xs

-- Might use this later when writing a property test after learning more about
-- QuickCheck. First need to learn: 1) how to randomly generate small lists
-- and 2) how to generate lists of even size using QC.
arrayPairSumBruteForce :: [Int] -> Int
arrayPairSumBruteForce = maximum . map findSum . permutations
  where
    findSum :: [Int] -> Int
    findSum = sum . map minimum . groupsOf 2
