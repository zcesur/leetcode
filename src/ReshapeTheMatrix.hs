module ReshapeTheMatrix where

import Util (groupsOf)

-- | Given a matrix represented by a two-dimensional array, and two positive
-- integers r and c representing the row number and column number of the
-- wanted reshaped matrix, reshape the matrix, filling with all the elements
-- of the original matrix in the same row-traversing order as they were. 
matrixReshape :: Int -> Int -> [[a]] -> [[a]]
matrixReshape _ _ [] = []
matrixReshape r' c' xss@(x:xs)
    | any ((/= c) . length) xs = error "Not a valid matrix"
    | r' * c' /= r * c       = xss
    | otherwise              = groupsOf c' $ concat xss
  where
    (r, c) = (length xss, length x)
