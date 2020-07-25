module DistributeCandies where

import           Data.List                      ( nub )

-- | Given an integer array with even length, where different numbers in this
-- array represent different kinds of candies, return the maximum number of
-- kinds of candies the sister could gain with the condition that the candies
-- are distributed equally in number to brother and sister.
distributeCandies :: [Int] -> Int
distributeCandies xs = length . take len . nub $ xs
  where len = length xs `div` 2
