module SingleElementInASortedArray where

import Data.Array

-- | Given a sorted array consisting of only integers where every element
-- appears twice except for one element which appears once. Find this single
-- element that appears only once.
singleNonDuplicate :: Array Int Int -> Int
singleNonDuplicate xs = go xs $ bounds xs
  where
    go xs (i,j)
        | i == j                    = xs ! i
        | even $ (nElems-1) `div` 2 = goEven xs (i,j)
        | otherwise                 = goOdd xs (i,j)
      where
        nElems = j-i+1

    goEven xs (i,j)
        | xs ! mid /= xs ! (mid-1) &&
          xs ! mid /= xs ! (mid+1) = xs ! mid
        | xs ! mid == xs ! (mid-1) = go xs (i, mid-2)
        | otherwise                = go xs (mid+2, j)
      where
        mid = (i+j) `div` 2

    goOdd xs (i,j)
        | xs ! mid == xs ! (mid-1) = go xs (mid+1, j)
        | otherwise                = go xs (i, mid-1)
      where
        mid = (i+j) `div` 2
