module KeyboardRow where

import           Data.Char                      ( toLower )

-- | Given a List of words, return the words that can be typed using letters
-- of alphabet on only one row of American keyboard.
findWords :: [String] -> [String]
findWords = filter wordFromSameRow

-- | Determine if a single word can be typed using letters of alphabet on only
-- one row of American keyboard.
wordFromSameRow :: String -> Bool
wordFromSameRow []       = True -- vacuously true
wordFromSameRow (x : xs) = all (`elem` row) xs'
 where
  row        = head $ filter (x' `elem`) keyboard
  (x' : xs') = map toLower (x : xs)
  keyboard   = ["qwertyuiop", "asdfghjkl", "zxcvbnm"]
