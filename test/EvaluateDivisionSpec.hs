module EvaluateDivisionSpec where

import EvaluateDivision
import Test.Hspec

spec :: Spec
spec = do
  describe "calcEquation" $ do
    it "can return the answers for the given queries, given some eqns" $ do
      let equations = [["a", "b"],["b", "c"]]
          values = [2.0, 3.0]
          queries = [["a", "c"],["b", "a"],["a", "e"],["a", "a"],["x", "x"]]

      calcEquation equations values queries `shouldBe`  [6, 0.5, -1, 1, -1]
