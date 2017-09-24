module MapSumPairs where

import qualified Data.Map as M
import           Data.Map ((!?))

-- Implement a MapSum class with insert, and sum methods.
-- 
-- For the method insert, you'll be given a pair of (string, integer). The
-- string represents the key and the integer represents the value. If the key
-- already existed, then the original key-value pair will be overridden to
-- the new one.
-- 
-- For the method sum, you'll be given a string representing the prefix, and
-- you need to return the sum of all the pairs' value whose key starts with
-- the prefix.

data Trie a = Trie { value :: Maybe a
                   , children :: M.Map Char (Trie a) }

empty :: Trie a
empty = Trie {value = Nothing, children = M.empty}

tinsert :: String -> a -> Trie a -> Trie a
tinsert "" v t = t { value = Just v }
tinsert (k:ks) v t = case children t !? k of
    Just t' -> t { children = M.insert k (tinsert ks v t') (children t) }
    Nothing -> t { children = M.insert k (tinsert ks v empty) (children t) }

tsum :: (Num a) => String -> Trie a -> a
tsum "" t = sum (map (tsum "") (M.elems (children t))) + case value t of
    Just v -> v
    Nothing -> 0
tsum (x:xs) t = case children t !? x of
    Just t' -> tsum xs t'
    Nothing -> 0
