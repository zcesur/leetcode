module ReverseWordsInAStringIII where

-- | Given a string, you need to reverse the order of characters in each word
-- within a sentence while still preserving whitespace and initial word order.
reverseWords :: String -> String
reverseWords = unwords . map reverse . words
