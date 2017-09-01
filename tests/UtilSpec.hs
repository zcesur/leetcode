module UtilSpec where

import Test.Hspec
import Util

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

  describe "fromBinary" $ do
    it "can convert binary to decimal" $ do
      fromBinary [1]     `shouldBe` 1
      fromBinary [0,1]   `shouldBe` 2
      fromBinary [1,1]   `shouldBe` 3 
      fromBinary [0,0,1] `shouldBe` 4
      fromBinary [1,0,1] `shouldBe` 5
      fromBinary [0,1,1] `shouldBe` 6
      fromBinary [1,1,1] `shouldBe` 7
