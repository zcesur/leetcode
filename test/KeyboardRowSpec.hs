module KeyboardRowSpec where

import           Test.Hspec
import           KeyboardRow

spec :: Spec
spec = do
  describe "findWords" $ do
    it "can filter for the words that can be typed using one keyboard row" $ do
      findWords ["Hello", "Alaska", "Dad", "Peace"] `shouldBe` ["Alaska", "Dad"]
