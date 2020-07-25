module FindLargestValueInEachTreeRow where

import           ADT.Tree

largestValues :: Ord a => Tree a -> [a]
largestValues t = go [t]
 where
  go [] = []
  go ts = maximum (map val ts) : go (concatMap children ts)
