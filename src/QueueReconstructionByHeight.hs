module QueueReconstructionByHeight where

import Data.List (sortBy)
import Data.Monoid ((<>))

-- | Suppose you have a random list of people standing in a queue. Each
-- person is described by a pair of integers (h, k), where h is the height of
-- the person and k is the number of people in front of this person who have
-- a height greater than or equal to h. Write an algorithm to reconstruct the
-- queue.
reconstruct :: [(Int, Int)] -> [(Int, Int)]
reconstruct = foldr go [] . sortBy (ordering)
  where
    ordering (a,b) (c,d) = a `compare` c <> d `compare` b
    go (h,k) acc = let (ls, rs) = splitAt k acc in ls ++ [(h,k)] ++ rs
