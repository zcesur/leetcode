module ArithmeticSlices where

import qualified Data.Array as A
import           Data.Array ((!))
import           Data.Maybe (isJust)

numberOfArithmeticSlices xs = counts ! (1, n)
  where
    n = length xs
    bounds = ((1,1), (n,n))    
    
    xs' = A.listArray (1,n) xs
    diffs  = A.listArray bounds [d i j | (i,j) <- A.range bounds]
    counts = A.listArray bounds [c i j | (i,j) <- A.range bounds]

    d i j | i+2 == j &&      d1 == d2 = Just d1
          | i+2  < j && Just d1 == ds = ds
          | otherwise = Nothing
          where d1 = xs'!(i+1) - xs'!i
                d2 = xs'!j - xs'!(j-1)
                ds = diffs!(i+1,j)

    c i j | i+2 == j = case d i j of
                Just _  -> 1
                Nothing -> 0
          | i+2 < j = counts!(i+1,j)
                    + length [diffs!(i,k) | k <- [i+1 .. j]
                                          , isJust $ diffs!(i,k)]
          | otherwise = 0
