module ReverseString where

-- | Takes a string as input and returns the string reversed.
reverseString :: String -> String
reverseString = foldl (flip (:)) []
