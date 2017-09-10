module NQueens where

import System.Environment (getArgs)
import Control.Monad (guard)
import Data.List (intercalate, (\\))

-- | The n-queens puzzle is the problem of placing n queens on an n×n
-- chessboard such that no two queens attack each other.
--
-- Given an integer n, return all distinct solutions to the n-queens puzzle.
queens :: Int -> [[Int]]
queens n = go n
  where
    go :: Int -> [[Int]]
    go 0 = return []
    go k = do qs <- go (k-1)
              q <- [1..n] \\ qs
              let (x, y) = (q, k); xys = zip qs [k-1, k-2 ..]
              guard (and $ [abs (x-x') /= abs (y-y') | (x', y') <- xys])
              return (q:qs)

main :: IO ()
main = do
    n <- read <$> head <$> getArgs
    mapM_ putStrLn $ intercalate [take n $ repeat '='] $ decode $ queens n
  where
    decode qs = let n = length $ head qs in map (map (f n)) qs
    f n i = take (i-1) (repeat '.') ++ ['♛'] ++ take (n-i) (repeat '.')
