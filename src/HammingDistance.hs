module HammingDistance where

-- | Compute the Hamming distance of 2 integers.
hammingDistance :: Int -> Int -> Int
hammingDistance x y = sum $ zipWith diff (rpad maxLen x') (rpad maxLen y')
  where
    diff x y = abs (x-y)
    x' = toBinary x
    y' = toBinary y
    maxLen = max (length x') (length y')

-- | Convert an integer to its binary representation. Note that the order is
-- reversed, but this is actually quite handy since it allows us to write
-- functions that use streaming operations, e.g. pad with zeros, convert back
-- to decimal etc. without the need of reversing the list.
toBinary :: Int -> [Int]
toBinary 0 = []
toBinary x = (x `mod` 2) : toBinary (x `div` 2)

-- | Right-pad a list of integers with zeros.
rpad :: Int -> [Int] -> [Int]
rpad n xs
    | n <= length xs = xs -- ^ For the sake of completeness.
    | otherwise      = take n . (++ repeat 0) $ xs
