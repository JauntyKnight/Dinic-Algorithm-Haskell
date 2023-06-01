module FlowGraph where

import Data.Array as Array
import qualified Data.IntMap.Strict as Map
import Data.IntMap (IntMap)

type Vertex = Int
type VertexMap a = Map.IntMap a
type Capacity = Float
type Flow = Float
type Edge = (Vertex, Vertex, Capacity, Flow)
type Path = [Vertex]
type Graph = VertexMap (VertexMap (Capacity, Flow))

neighbors :: Graph -> Vertex -> [(Vertex, (Capacity, Flow))]
neighbors g v = Map.assocs (g Map.! v)

addFlow :: Graph -> Vertex -> Vertex -> Flow -> Graph
addFlow g u v f = Map.adjust (Map.adjust (\(c, f') -> (c, f' + f)) v) u g

-- removeEdgeCascade :: Graph -> Vertex -> Vertex -> Graph
-- removeEdgeCascade g u v 
--     | Map.null (g Map.! u) = removeVertexCascade g u
--     | otherwise = Map.adjust (Map.delete v) u g
--     where 
--         g' = removeEdgeCascade g v u


removeEdge :: Graph -> Vertex -> Vertex -> Graph
removeEdge g u v = Map.adjust (Map.delete v) u g

restorePath :: VertexMap Vertex -> Vertex -> Vertex -> Maybe Path
restorePath parents s t
    | s == t = Just []
    | not $ Map.member t parents = Nothing
    | otherwise = case restorePath parents s (parents Map.! t) of
        Nothing -> Nothing
        Just p -> Just (t:p)
