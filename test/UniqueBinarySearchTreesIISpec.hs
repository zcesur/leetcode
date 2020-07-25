module UniqueBinarySearchTreesIISpec where

import           UniqueBinarySearchTreesII
import           ADT.Tree
import           Test.Hspec

spec :: Spec
spec = do
  describe "generateTrees" $ do
    it "can generate all structurally unique BSTs that store values 1..n" $ do
      -- https://oeis.org/A000108
      let catalanNumbers = [1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862, 16796]
      map (length . generateTrees) [0 .. 10] `shouldBe` catalanNumbers
