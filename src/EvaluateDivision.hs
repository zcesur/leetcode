module EvaluateDivision where

import qualified Data.Graph.Inductive          as Graph
import           Data.Graph.Inductive           ( Gr
                                                , Node
                                                , LNode
                                                , LEdge
                                                , match
                                                )

import qualified Data.Map                      as Map
import           Data.Map                       ( Map
                                                , (!)
                                                )

import           Data.List                      ( nub
                                                , sort
                                                )

-- | Equations are given in the format A / B = k, where A and B are variables
-- represented as strings, and k is a real number (floating point number).
-- Given some queries, return the answers. If the answer does not exist,
-- return -1.0.
calcEquation :: [(String, String)] -> [Float] -> [(String, String)] -> [Float]
calcEquation eqn vals qs = map (handleEmpty . uncurry (go graph)) qs'
 where
  go :: Gr a Float -> Node -> Node -> [Float]
  go g s t | t `notElem` Graph.nodes g = []
           | s == t                    = [1]
  go g s t = case match s g of
    (Just c , g') -> [ l * rest | (s', l) <- Graph.lsuc' c, rest <- go g' s' t ]
    (Nothing, _ ) -> []

  (nodes, edges, nodeMap) = parse eqn vals
  graph                   = buildGraph nodes edges
  qs' = map (\(x, y) -> (lookup' x nodeMap, lookup' y nodeMap)) qs

  lookup'                 = Map.findWithDefault 0

  handleEmpty []      = -1
  handleEmpty (x : _) = x

buildGraph :: [LNode String] -> [LEdge Float] -> Gr String Float
buildGraph nodes edges = Graph.mkGraph nodes edges'
 where
  edges'  = edges ++ idEdges ++ opEdges
  idEdges = map (\(x, _) -> (x, x, 1)) nodes
  opEdges = map (\(x, y, z) -> (y, x, 1 / z)) edges

parse
  :: [(String, String)]
  -> [Float]
  -> ([LNode String], [LEdge Float], Map String Int)
parse eqns vals = (nodes, edges, nodeMap)
 where
  edgesStr = zipWith (\(x, y) z -> (x, y, z)) eqns vals
  nodesStr = nub $ sort $ foldl (\acc (x, y, _) -> x : y : acc) [] edgesStr

  nodes    = zip [1 ..] nodesStr
  edges    = map (\(x, y, z) -> (nodeMap ! x, nodeMap ! y, z)) edgesStr

  nodeMap  = Map.fromList $ zip nodesStr [1 ..]
