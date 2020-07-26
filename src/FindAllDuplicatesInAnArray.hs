{-# LANGUAGE ScopedTypeVariables #-}

module FindAllDuplicatesInAnArray where

import           Control.Monad                  ( foldM )
import           Control.Monad.ST
import           Data.Array.ST

-- | Given an array of integers, 1 ≤ a[i] ≤ n (n = size of array), some
-- elements appear twice and others appear once.
-- 
-- Find all the elements that appear twice in this array.
-- 
-- Could you do it without extra space and in O(n) runtime?
findDuplicates :: [Int] -> [Int]
findDuplicates xs = runST (findDuplicates' xs)
 where
  findDuplicates' :: forall s . [Int] -> ST s [Int]
  findDuplicates' ys = do
    arr <- newListArray (1, length ys) ys :: ST s (STArray s Int Int)

    let go :: [Int] -> Int -> ST s [Int]
        go acc i = do
          x <- abs <$> readArray arr i
          y <- readArray arr x
          if y < 0
            then return (x : acc)
            else do
              writeArray arr x (-y)
              return acc

    foldM go [] [1 .. length ys]
