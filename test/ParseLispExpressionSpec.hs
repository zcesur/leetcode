module ParseLispExpressionSpec where

import           Test.Hspec
import           ParseLispExpression

spec :: Spec
spec = do
  describe "parseEval" $ do
    it "can evaluate the given expression" $ do
      parseEval "(add 1 2)" `shouldBe` Right (Just 3)
      parseEval "(mult 3 (add 2 3))" `shouldBe` Right (Just 15)
      parseEval "(let x 2 (mult x 5))" `shouldBe` Right (Just 10)
      parseEval "(let x 2 (mult x (let x 3 y 4 (add x y))))" `shouldBe` Right (Just 14)
      parseEval "(let x 3 x 2 x)" `shouldBe` Right (Just 2)
      parseEval "(let x 1 y 2 x (add x y) (add x y))" `shouldBe` Right (Just 5)
      parseEval "(let x 2 (add (let x 3 (let x 4 x)) x))" `shouldBe` Right (Just 6)
      parseEval "(let a1 3 b2 (add a1 1) b2)" `shouldBe` Right (Just 4)
