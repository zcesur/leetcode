module ParsingABooleanExpressionSpec where

import Test.Hspec
import ParsingABooleanExpression
import Data.Either

spec :: Spec
spec = do
  describe "eval" $ do
    it "can evaluate the given expression" $ do
      parseEval "!(f)" `shouldBe` Right True
      parseEval "|(f,t)" `shouldBe` Right True
      parseEval "&(t,f)" `shouldBe` Right False
      parseEval "|(&(t,f,t),!(t))" `shouldBe` Right False
