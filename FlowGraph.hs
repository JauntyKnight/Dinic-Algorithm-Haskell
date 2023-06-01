module FlowGraph where

import Data.Array as Array
import qualified Data.IntMap.Strict as Map
import Data.IntMap (IntMap)
import Data.List

type Vertex = Int
type VertexMap a = Map.IntMap a
type Capacity = Float
type Flow = Float
type Edge = (Vertex, Vertex, Capacity, Flow)
type Path = [Vertex]
type Graph = VertexMap (VertexMap (Capacity, Flow))


fromAssocList :: [(Vertex, [(Vertex, Capacity)])] -> Graph
fromAssocList assocl = Map.fromList [(v, Map.fromList $ map (\(u, c) -> (u, (c, c))) nbs) | (v, nbs) <- assocl]


fromEdgeList :: [(Vertex, Vertex, Capacity)] -> Graph
fromEdgeList edges = fromAssocList assocl where
    grouped = groupBy (\(u, _, _) (v, _, _) -> u == v) $ sortBy (\(u, _, _) (v, _, _) -> compare u v) edges
    fst3 (x, _, _) = x
    assocl = [(v, nbs) | group <- grouped, let v = fst3 $ head group, let nbs = map (\(_, w, c) -> (w, c)) group]


toEdgeList :: Graph -> [(Vertex, Vertex, Capacity, Flow)]
toEdgeList g = [(u, v, c, f) | (u, nbs) <- Map.assocs g, (v, (c, f)) <- Map.assocs nbs]

getEdge :: Graph -> Vertex -> Vertex -> Edge
getEdge g u v = (u, v, c, f) where
    (c, f) = (g Map.! u) Map.! v


neighbors :: Graph -> Vertex -> [(Vertex, (Capacity, Flow))]
neighbors g v = Map.assocs (g Map.! v)


pushFlow :: Graph -> Vertex -> Vertex -> Flow -> Graph
pushFlow g u v f = Map.adjust (Map.adjust (\(c, f') -> (c, f' + f)) v) u g


-- Assumes there are no cycles of size 2. Inshallah will be fixed later
constructResidual :: Graph -> Graph
constructResidual g = fromEdgeList $ edges ++ map (\(u, v, _) -> (v, u, 0)) edges where
    edges = map (\(u, v, c, f) -> (u, v, c)) $ toEdgeList g


removeEdge :: Graph -> Vertex -> Vertex -> Graph
removeEdge g u v = Map.adjust (Map.delete v) u g

restorePath :: VertexMap Vertex -> Vertex -> Vertex -> Maybe Path
restorePath parents s t
    | s == t = Just []
    | not $ Map.member t parents = Nothing
    | otherwise = case restorePath parents s (parents Map.! t) of
        Nothing -> Nothing
        Just p -> Just (t:p)

