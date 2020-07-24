module ReshapeTheMatrixSpec where

import Test.Hspec
import ReshapeTheMatrix

spec :: Spec
spec = do
  describe "matrixReshape" $ do
    it "reshape a matrix" $ do
      let m1 = [[1,2],[3,4]]
      let m2 = [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]

      matrixReshape 1 4 m1 `shouldBe` [[1,2,3,4]]
      matrixReshape 4 1 m1 `shouldBe` [[1],[2],[3],[4]]
      matrixReshape 2 3 m1 `shouldBe` m1

      matrixReshape 2 6 m2 `shouldBe` [[1,2,3,4,5,6],[7,8,9,10,11,12]]
      matrixReshape 3 4 m2 `shouldBe` [[1,2,3,4],[5,6,7,8],[9,10,11,12]]
      matrixReshape 1 1 m2 `shouldBe` m2
