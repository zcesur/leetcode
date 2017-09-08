module CountingBits where

import Data.Array

-- | Given a non negative integer number num. For every numbers i in the
-- range 0 ≤ i ≤ num calculate the number of 1's in their binary
-- representation and return them as an array.
countBits :: Int -> [Int]
countBits n = elems bits
  where
    bits = listArray (0, n) (0:map f [1..n])
    f i = 1 + bits ! (i - largestPowOf2LT i)
    largestPowOf2LT = (2^) . floor . logBase 2 . fromIntegral
