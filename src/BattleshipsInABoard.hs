module BattleshipsInABoard where

import           Data.Array
import           Control.Monad
import           Control.Monad.State

-- | Given an 2D board, count how many battleships are in it. The battleships
-- are represented with 'X's, empty slots are represented with '.'s. You may
-- assume the following rules:
-- 
--     You receive a valid board, made of only battleships or empty
-- slots.
--     Battleships can only be placed horizontally or vertically. In other
-- words, they can only be made of the shape 1xN (1 row, N columns) or Nx1 (N
-- rows, 1 column), where N can be of any size.
--     At least one horizontal or vertical cell separates between two
-- battleships - there are no adjacent battleships.
countBattleships :: [[Char]] -> Int
countBattleships xs = execState (go xs) 0
 where
  go xs =
    let (r, c) = (length xs, length $ head xs)
        xs'    = listArray ((1, 1), (r, c)) $ concat xs
    in  forM (range (bounds xs')) $ \(i, j) -> do
          if shipDetected xs' (i, j) then increment else return ()

  shipDetected xs (i, j) =
    not
      $  xs
      !  (i, j)
      == '.'
      || (i > 1 && xs ! (i - 1, j) == 'X')
      || (j > 1 && xs ! (i, j - 1) == 'X')

  increment :: State Int ()
  increment = state $ \c -> ((), c + 1)
