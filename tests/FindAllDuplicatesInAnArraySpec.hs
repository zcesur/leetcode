module FindAllDuplicatesInAnArraySpec where

import FindAllDuplicatesInAnArray
import Test.Hspec

spec :: Spec
spec = do
  describe "findDuplicates" $ do
    it "can find duplicates given a list of integers with 1 ≤ a[i] ≤ n" $ do
      findDuplicates [4,3,2,7,8,2,3,1] `shouldBe` [2,3]
