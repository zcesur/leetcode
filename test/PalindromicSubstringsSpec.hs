module PalindromicSubstringsSpec where

import           PalindromicSubstrings
import           Test.Hspec

spec :: Spec
spec = do
  describe "countSubstrings" $ do
    it "can count how many palindromic substrings there are" $ do
      countSubstrings "aaa" `shouldBe` 6
      countSubstrings "abc" `shouldBe` 3
