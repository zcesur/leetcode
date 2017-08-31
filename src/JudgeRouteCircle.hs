module JudgeRouteCircle where

data Direction = R | L | U | D deriving (Eq, Enum, Bounded, Show)

-- | Used to keep track of how many moves the robot made in each direction
type Moves = [(Direction, Int)]

-- | Judge if the robot makes a circle, i.e., if it moves back to the original
-- place, by aggregating its sequence of moves and checking if it made the same
-- number of moves in each of the opposite directions.
judgeCircle :: [Direction] -> Bool
judgeCircle = cancelsTo0 . foldl increment initial
  where
    initial :: Moves
    initial = map (\x -> (x,0)) [minBound :: Direction ..]

increment :: Moves -> Direction -> Moves
increment ms d = map (\(x,y) -> if x == d then (x,y+1) else (x,y)) ms

cancelsTo0 :: Moves -> Bool
cancelsTo0 ms = lookup R ms == lookup L ms && lookup U ms == lookup D ms
