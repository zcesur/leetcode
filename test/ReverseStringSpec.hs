module ReverseStringSpec where

import           Test.Hspec
import           ReverseString

spec :: Spec
spec = do
  describe "reverseString" $ do
    it "can reverse string" $ do
      reverseString "hello" `shouldBe` "olleh"
