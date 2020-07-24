module FindAllDuplicatesInAnArraySpec where

import FindAllDuplicatesInAnArray
import Test.Hspec
import Data.List (sort)

spec :: Spec
spec = do
  describe "findDuplicates" $ do
    it "can find duplicates given a list of integers with 1 ≤ a[i] ≤ n" $ do
      sort (findDuplicates [4,3,2,7,8,2,3,1]) `shouldBe` [2,3]
      sort (findDuplicates [6,6,3,4,5,1,2,5]) `shouldBe` [5,6]
