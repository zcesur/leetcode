module BattleshipsInABoardSpec where

import BattleshipsInABoard
import Test.Hspec

spec :: Spec
spec = do
  describe "countBattleships" $ do
    it "can count battleships on a given 2D board" $ do
      let boards = [[]

                  , [""
                    ,""]

                  , ["..."
                    ,"..."
                    ,"..."]
                    
                  , ["X..X"
                    ,"...X"
                    ,"...X"]

                  , ["X.X..XX"
                    ,"X..X..."
                    ,"X..X.XX"
                    ,".X....."
                    ,".X.XX.."
                    ,".....X."
                    ,".XXX..X"]]
      map countBattleships boards `shouldBe` [0,0,0,2,10]
