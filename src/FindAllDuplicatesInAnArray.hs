module FindAllDuplicatesInAnArray where

import Control.Monad.State

import Data.List (sort)

import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Lazy as HashMap
import Data.HashMap.Lazy (HashMap(..))

findDuplicates :: Ord a => [a] -> [a]
findDuplicates xs = go $ sort xs
  where
    go (x:y:ys) | x == y    = x : go ys
                | otherwise = go (y:ys)
    go _ = []

findDuplicates' :: (Eq a, Hashable a) => [a] -> [a]
findDuplicates' xs = evalState (go xs) HashMap.empty
  where
    go :: (Eq k, Hashable k) => [k] -> State (HashMap k Char) [k]
    go [] = return []
    go (x:xs) = do
        map <- get
        if x `HashMap.member` map
            then do pop x
                    liftM2 (:) (return x) (go xs)
            else do push x
                    go xs

    pop :: (Eq k, Hashable k) => k -> State (HashMap k Char) k 
    pop a = state $ \xs -> (a, HashMap.delete a xs)

    push :: (Eq k, Hashable k) => k -> State (HashMap k Char) ()
    push a = state $ \xs -> ((), HashMap.insert a ' ' xs)
