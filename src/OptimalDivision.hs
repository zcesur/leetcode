module OptimalDivision (optimalDivision) where

import qualified Data.Array as Array
import           Data.Array ((!), range)

-- | Given a list of positive integers, the adjacent integers will perform
-- the float division. For example, [2,3,4] -> 2 / 3 / 4.
-- 
-- However, you can add any number of parenthesis at any position to change
-- the priority of operations. You should find out how to add parenthesis to
-- get the maximum result, and return the corresponding expression in string
-- format. Your expression should NOT contain redundant parenthesis.
optimalDivision :: [Float] -> String
optimalDivision xs = showTable ! (1,n)
  where
    n = length xs
    bounds = ((1,1), (n,n))
    xs' = Array.listArray (1,n) xs

    showTable = Array.listArray bounds [show' Max i j | (i,j) <- range bounds]
    show' m i j
        | i == j    = show $ floor $ xs'!i
        | k+1 == j  = expr1 ++ "/" ++ expr2
        | i == k    = expr1 ++ "/(" ++ expr2 ++ ")"
        | otherwise = "(" ++ expr1 ++ ")/(" ++ expr2 ++ ")"
        where expr1 = show' m i k
              expr2 = show' (other m) (k+1) j
              k     = snd $ (table m)!(i,j)

    maxTable = Array.listArray bounds [fmax i j | (i,j) <- range bounds]
    minTable = Array.listArray bounds [fmin i j | (i,j) <- range bounds]
    table Max = maxTable
    table Min = minTable

    fmax i j
        | i == j = (xs'!i, i)
        | i < j  = maximum $
            [(fst (maxTable!(i,k)) / fst (minTable!(k+1,j)), k) |
            k <- [i..j-1]]
    fmin i j
        | i == j = (xs'!i, i)
        | i < j  = minimum $
            [(fst (minTable!(i,k)) / fst (maxTable!(k+1,j)), k) |
            k <- [i..j-1]]

data Table = Min | Max
other Min = Max
other Max = Min
