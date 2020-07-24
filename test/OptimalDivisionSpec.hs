module OptimalDivisionSpec where

import OptimalDivision
import Test.Hspec

spec :: Spec
spec = do
  describe "optimalDivision" $ do
    it "can return the expression that results in the largest division\
       \without any redundant parens" $ do
      
      let xs = [1000,100,10,2] :: [Float]
      optimalDivision xs `shouldBe` "1000/(100/10/2)"
