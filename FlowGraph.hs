module FlowGraph where

import Data.Array as Array
import qualified Data.IntMap.Strict as Map
import Data.IntMap (IntMap)
import Data.List
import Debug.Trace
import System.IO

type Vertex = Int
type VertexMap a = Map.IntMap a
type Capacity = Int
type Flow = Int
type Edge = (Vertex, Vertex, Capacity)
type Path = [Vertex]
type FlowGraph = VertexMap (VertexMap Capacity)


fromAssocList :: [(Vertex, [(Vertex, Capacity)])] -> FlowGraph
fromAssocList assocl = Map.fromList [(v, Map.fromList nbs) | (v, nbs) <- assocl]


fromEdgeList :: [Edge] -> FlowGraph
fromEdgeList edges = fromAssocList assocl where
    grouped = groupBy (\(u, _, _) (v, _, _) -> u == v) $ sortBy (\(u, _, _) (v, _, _) -> compare u v) edges
    fst3 (x, _, _) = x
    assocl = [(v, nbs) | group <- grouped, let v = fst3 $ head group, let nbs = map (\(_, w, c) -> (w, c)) group]


toEdgeList :: FlowGraph -> [Edge]
toEdgeList g = [(u, v, c) | (u, nbs) <- Map.assocs g, (v, c) <- Map.assocs nbs]

getEdge :: FlowGraph -> Vertex -> Vertex -> Edge
getEdge g u v = (u, v, c) where
    c = (g Map.! u) Map.! v


neighbors :: FlowGraph -> Vertex -> [(Vertex, Capacity)]
neighbors g v
    | Map.member v g = Map.assocs (g Map.! v)
    | otherwise = []


pushFlow :: FlowGraph -> Vertex -> Vertex -> Flow -> FlowGraph
pushFlow g u v f = Map.adjust (Map.adjust (subtract f) v) u g' where
    g' = Map.adjust (Map.adjust (+ f) u) v g


pushFlowLayered :: FlowGraph -> Vertex -> Vertex -> Flow -> FlowGraph
pushFlowLayered g u v f = Map.adjust (Map.adjust (\c -> c - f) v) u g


-- Assumes there are no cycles of size 2, otherwise it may yield a wrong result
constructResidual :: FlowGraph -> FlowGraph
constructResidual g = fromEdgeList $ edges ++ map (\(u, v, c) -> (v, u, 0)) edges where
    edges = toEdgeList g


removeEdge :: FlowGraph -> Vertex -> Vertex -> FlowGraph
removeEdge g u v = Map.adjust (Map.delete v) u g

restorePath :: VertexMap Vertex -> Vertex -> Vertex -> Maybe Path
restorePath parents s t
    | s == t = Just [s]
    | not $ Map.member t parents = Nothing
    | otherwise = (t:) <$> restorePath parents s (parents Map.! t)

-- reads a FlowGraph from a file in the format:
-- n s t -- number of edges, source and sink on the first line
-- u v c -- edge from u to v with capacity c on the remaining lines
readFileEdgeList :: String -> IO (Vertex, Vertex, [Edge])
readFileEdgeList filename = do
        handle <- openFile filename ReadMode
        contents <- hGetContents handle
        let ls = words contents
            [n, s, t] = map read $ take 3 ls
            edges = map (\(u, v, c) -> (read u, read v, read c)) . group3 $ drop 3 ls
        return (s, t, edges)
        where group3 [] = []
              group3 (x:y:z:xs) = (x, y, z) : group3 xs
