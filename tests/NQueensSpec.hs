module NQueensSpec where

import NQueens
import Test.Hspec

spec :: Spec
spec = do
  describe "queens" $ do
    it "can generate all distinct solutions to the n-queens puzzle" $ do
      let a000170 = [1, 0, 0, 2, 10, 4, 40, 92, 352, 724]
      map (length . queens) [1..10] `shouldBe` a000170
