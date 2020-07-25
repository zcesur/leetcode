module ArithmeticSlicesSpec where

import           ArithmeticSlices
import           Test.Hspec

spec :: Spec
spec = do
  describe "numberOfArithmeticSlices" $ do
    it "can return the number of arithmetic slices in the list" $ do
      numberOfArithmeticSlices [1 .. 6] `shouldBe` 10
      numberOfArithmeticSlices [1, 2, 3, 4, 3, 2, 1] `shouldBe` 6
      numberOfArithmeticSlices [1, 2, 3, 6, 9, 12] `shouldBe` 4
