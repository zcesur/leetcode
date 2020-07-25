module SingleElementInASortedArraySpec where

import           SingleElementInASortedArray
import           Data.Array
import           Test.Hspec

spec :: Spec
spec = do
  describe "singleNonDuplicate" $ do
    it "can find the single element that appears only once in the array" $ do
      let
        xs = listArray (1, 9) [1, 1, 2, 3, 3, 4, 4, 8, 8] :: Array Int Int
        ys =
          listArray (1, 13) [1, 1, 2, 2, 3, 4, 4, 5, 5, 6, 6, 7, 7] :: Array
              Int
              Int
      singleNonDuplicate xs `shouldBe` 2
      singleNonDuplicate ys `shouldBe` 3
