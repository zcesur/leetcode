module Util where

-- | Convert an integer to its binary representation. Note that the order is
-- reversed, but this is actually quite handy since it allows us to write
-- functions that use streaming operations, e.g. pad with zeros, convert back
-- to decimal etc. without the need of reversing the list.
toBinary :: Int -> [Int]
toBinary 0 = []
toBinary x = (x `mod` 2) : toBinary (x `div` 2)

-- | Convert binary to decimal.
fromBinary :: [Int] -> Int
fromBinary = sum . zipWith (*) powersOf2
 where
  powersOf2 = map (pow 2) [0 ..]
  pow       = (^) :: Int -> Int -> Int


-- | Split a list into sublists of size n (except for the last one)
groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)
