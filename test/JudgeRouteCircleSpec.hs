module JudgeRouteCircleSpec where

import Test.Hspec
import JudgeRouteCircle

spec :: Spec
spec = do
  describe "increment" $ do
    it "can increment a list of moves given a direction" $ do
        let xs  = [(L,3), (U,2), (D,1), (R,4)] 
        let xs' = [(L,3), (U,3), (D,1), (R,4)]
        increment xs U `shouldBe` xs'

  describe "cancelsTo0" $ do
    it "can determine if moves along each axis cancel out" $ do
        cancelsTo0 [(U,3), (L,4), (R,4), (D,3)] `shouldBe` True
        cancelsTo0 [(R,3), (L,2), (U,1), (D,1)] `shouldBe` False

  describe "judgeCircle" $ do
    it "can determine if the sequence of moves lead back to origin" $ do
      judgeCircle [U, D] `shouldBe` True
      judgeCircle [R, L] `shouldBe` True
      judgeCircle [R, L, U, D] `shouldBe` True
      judgeCircle [R, U, L, L, D, R] `shouldBe` True

      judgeCircle [L, L] `shouldBe` False
      judgeCircle [U, U] `shouldBe` False
      judgeCircle [U, D, R, R] `shouldBe` False
      judgeCircle [D, L, R, D] `shouldBe` False
      judgeCircle [R, U, L, L] `shouldBe` False
