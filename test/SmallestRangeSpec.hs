module SmallestRangeSpec where

import           SmallestRange
import           Test.Hspec

spec :: Spec
spec = do
  describe "smallestRange" $ do
    it
        "can find the smallest range that includes at least 1 number from each\
       \of the lists"
      $ do
          let xs = [[4, 10, 15, 24, 26], [0, 9, 12, 20], [5, 18, 22, 30]]
          smallestRange xs `shouldBe` (20, 24)
