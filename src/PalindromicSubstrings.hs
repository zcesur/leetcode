module PalindromicSubstrings where

import qualified Data.Array as Array
import           Data.Array ((!))

countSubstrings :: String -> Int
countSubstrings xs = cs ! (1,n)
  where
    n = length xs
    bounds = ((1,1), (n,n))
    xs' = Array.listArray (1,n) xs

    cs = Array.listArray bounds [count i j | (i,j) <- Array.range bounds]
    ps = Array.listArray bounds [palindrome i j | (i,j) <- Array.range bounds]
    
    count i j | i == j    = 1
              | otherwise = sum [fromBool $ palindrome i k | k <- [i..j]]
                          + cs!(i+1, j)

    palindrome i j | i   == j  = True
                   | i+1 == j  = xs'!i == xs'!j
                   | otherwise = ps!(i+1, j-1) && xs'!i == xs'!j

    fromBool True = 1
    fromBool False = 0
