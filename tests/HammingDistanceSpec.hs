module HammingDistanceSpec where

import Test.Hspec
import HammingDistance

spec :: Spec
spec = do
  describe "toBinary" $ do
    it "can convert a number to binary" $ do
      toBinary 1 `shouldBe` [1]
      toBinary 2 `shouldBe` [0,1]
      toBinary 3 `shouldBe` [1,1]
      toBinary 4 `shouldBe` [0,0,1]
      toBinary 5 `shouldBe` [1,0,1]
      toBinary 6 `shouldBe` [0,1,1]
      toBinary 7 `shouldBe` [1,1,1]
  
  describe "rpad" $ do
    it "can right-pad a list of integers with zeros" $ do
      rpad 2 [] `shouldBe` [0,0]
      rpad 2 [1,2,3] `shouldBe` [1,2,3]
      rpad 3 [1,2,3] `shouldBe` [1,2,3]
      rpad 5 [1,2,3] `shouldBe` [1,2,3,0,0]

  describe "hammingDistance" $ do
    it "can find the Hamming distance of 2 integers" $ do
      hammingDistance 1 2 `shouldBe` 2
      hammingDistance 1 4 `shouldBe` 2
      hammingDistance 6 7 `shouldBe` 1
