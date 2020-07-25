module SmallestRange where

import qualified Data.Map.Lazy                 as Map
import           Data.Map.Lazy                  ( Map(..) )

import           Control.Monad.State

-- | You have k lists of sorted integers in ascending order. Find the
-- smallest range that includes at least one number from each of the k lists.
--
-- We define the range [a,b] is smaller than range [c,d] if b-a < d-c or a <
-- c if b-a == d-c.
smallestRange :: [[Int]] -> (Int, Int)
smallestRange xs = evalState (go (0, maxBound :: Int)) xs'
 where
  xs' = Map.fromList $ zip xs (repeat ' ')

  go :: (Int, Int) -> State (Map [Int] Char) (Int, Int)
  go range = do
    (mins, _) <- popMin

    if null mins
      then return range
      else do
        let (min : rest) = mins
        maxS <- peekMax
        push (rest, ' ')
        go $ chooseRange range (min, head (fst maxS))

popMin :: State (Map k a) (k, a)
popMin = state Map.deleteFindMin

peekMax :: State (Map k a) (k, a)
peekMax = state $ \xs -> (Map.findMax xs, xs)

push :: Ord k => (k, a) -> State (Map k a) ()
push x = state $ \xs -> ((), uncurry Map.insert x xs)

chooseRange :: (Int, Int) -> (Int, Int) -> (Int, Int)
chooseRange (a, b) (c, d) =
  case compare (b - a) (d - c) `mappend` compare a c of
    GT -> (c, d)
    _  -> (a, b)
