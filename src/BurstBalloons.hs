module BurstBalloons where

import Data.Array

-- | Find the maximum coins you can collect by bursting the balloons, where
-- collecting coins works as follows.
--
-- Given n balloons, indexed from 0 to n-1. Each balloon is painted with a
-- number on it represented by array nums. You are asked to burst all the
-- balloons. If the you burst balloon i you will get nums[left] * nums[i] *
-- nums[right] coins. Here left and right are adjacent indices of i. After
-- the burst, the left and right then becomes adjacent.
maxCoins :: [Int] -> Int
maxCoins xs = table ! (1, n)
  where
    n = length xs

    xs' = listArray (0, n+1) $ [1] ++ xs ++ [1]
    table = listArray ((0, 0), (n+1, n+1)) [f i j | i <- [0..n+1]
                                                  , j <- [0..n+1]]

    f i j | i <= j  = maximum $ map (valueOf (i, j)) [i..j]
          | i > j   = 0

    valueOf (i, j) k = xs' ! (i-1) * xs' ! k * xs' ! (j+1) +
                       table ! (i, k-1) + table ! (k+1, j)
