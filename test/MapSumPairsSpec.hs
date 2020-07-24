module MapSumPairsSpec where

import MapSumPairs
import Test.Hspec

import Control.Monad.State

insert :: (String, a) -> State (Trie a) ()
insert x = state $ \xs -> ((), uncurry tinsert x xs)

sum' :: Num a => String -> State (Trie a) a
sum' x = state $ \xs -> (tsum x xs, xs)

spec :: Spec
spec = do
  describe "sum" $ do
    it "can return the sum of all the pairs' value whose key starts with the\
       \prefix." $ do
      let test1 = do insert ("apple", 3)
                     sum' "ap"
          test2 = do insert ("apple", 3)
                     insert ("app", 2)
                     sum' "ap"

      evalState test1 empty `shouldBe` 3
      evalState test2 empty `shouldBe` 5
