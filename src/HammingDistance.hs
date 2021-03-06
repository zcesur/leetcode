module HammingDistance where

import           Util                           ( toBinary )

-- | Compute the Hamming distance of 2 integers.
hammingDistance :: Int -> Int -> Int
hammingDistance x y = sum $ zipWith diff (rpad maxLen x') (rpad maxLen y')
 where
  diff a b = abs (a - b)
  x'     = toBinary x
  y'     = toBinary y
  maxLen = max (length x') (length y')

-- | Right-pad a list of integers with zeros.
rpad :: Int -> [Int] -> [Int]
rpad n xs | n <= length xs = xs
          | otherwise      = take n . (++ repeat 0) $ xs
