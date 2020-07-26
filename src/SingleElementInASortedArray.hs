module SingleElementInASortedArray where

import           Data.Array

-- | Given a sorted array consisting of only integers where every element
-- appears twice except for one element which appears once. Find this single
-- element that appears only once.
singleNonDuplicate :: Array Int Int -> Int
singleNonDuplicate arr = go arr $ bounds arr
 where
  go xs (i, j) | i == j                      = xs ! i
               | even $ (nElems - 1) `div` 2 = goEven xs (i, j)
               | otherwise                   = goOdd xs (i, j)
    where nElems = j - i + 1

  goEven ys (i, j)
    | ys ! mid /= ys ! (mid - 1) && ys ! mid /= ys ! (mid + 1) = ys ! mid
    | ys ! mid == ys ! (mid - 1) = go ys (i, mid - 2)
    | otherwise                  = go ys (mid + 2, j)
    where mid = (i + j) `div` 2

  goOdd zs (i, j) | zs ! mid == zs ! (mid - 1) = go zs (mid + 1, j)
                  | otherwise                  = go zs (i, mid - 1)
    where mid = (i + j) `div` 2
