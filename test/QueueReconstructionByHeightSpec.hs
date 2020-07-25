module QueueReconstructionByHeightSpec where

import           QueueReconstructionByHeight
import           Test.Hspec

spec :: Spec
spec = do
  describe "reconstruct" $ do
    it "can reconstruct a queue" $ do
      let xs  = [(7, 0), (4, 4), (7, 1), (5, 0), (6, 1), (5, 2)]
          xs' = [(5, 0), (7, 0), (5, 2), (6, 1), (4, 4), (7, 1)]
      reconstruct xs `shouldBe` xs'
