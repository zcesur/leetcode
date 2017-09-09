module SmallestRange where

import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map(..))

import Control.Monad.State

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
                let (min:rest) = mins
                (max:_, _) <- peekMax
                push (rest, ' ')
                go $ chooseRange range (min, max)

popMin :: State (Map k a) (k,a)
popMin = state Map.deleteFindMin

peekMax :: State (Map k a) (k,a)
peekMax = state $ \xs -> (Map.findMax xs, xs)

push :: Ord k => (k,a) -> State (Map k a) ()
push x = state $ \xs -> ((), uncurry Map.insert x xs)

chooseRange :: (Int, Int) -> (Int, Int) -> (Int, Int)
chooseRange (a,b) (c,d) = case compare (b-a) (d-c) `mappend` compare a c of
    GT -> (c,d)
    _  -> (a,b)
