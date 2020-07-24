module ReverseWordsInAStringIIISpec where

import Test.Hspec
import ReverseWordsInAStringIII

spec :: Spec
spec = do
  describe "reverseWords" $ do
    it "can reverse the order of chars in each word within a sentence" $ do
      let xs  = "Let's take LeetCode contest"
      let xs' = "s'teL ekat edoCteeL tsetnoc"
      reverseWords xs `shouldBe` xs'
