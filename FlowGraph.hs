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
type Graph = VertexMap (VertexMap Capacity)


fromAssocList :: [(Vertex, [(Vertex, Capacity)])] -> Graph
fromAssocList assocl = Map.fromList [(v, Map.fromList $ map (\(u, c) -> (u, c)) nbs) | (v, nbs) <- assocl]


fromEdgeList :: [Edge] -> Graph
fromEdgeList edges = fromAssocList assocl where
    grouped = groupBy (\(u, _, _) (v, _, _) -> u == v) $ sortBy (\(u, _, _) (v, _, _) -> compare u v) edges
    fst3 (x, _, _) = x
    assocl = [(v, nbs) | group <- grouped, let v = fst3 $ head group, let nbs = map (\(_, w, c) -> (w, c)) group]


toEdgeList :: Graph -> [Edge]
toEdgeList g = [(u, v, c) | (u, nbs) <- Map.assocs g, (v, c) <- Map.assocs nbs]

getEdge :: Graph -> Vertex -> Vertex -> Edge
getEdge g u v = (u, v, c) where
    c = (g Map.! u) Map.! v


neighbors :: Graph -> Vertex -> [(Vertex, Capacity)]
neighbors g v
    | Map.member v g = Map.assocs (g Map.! v)
    | otherwise = []


pushFlow :: Graph -> Vertex -> Vertex -> Flow -> Graph
pushFlow g u v f = Map.adjust (Map.adjust (\c -> c - f) v) u g' where
    g' = Map.adjust (Map.adjust (+ f) u) v g


pushFlowLayered :: Graph -> Vertex -> Vertex -> Flow -> Graph
pushFlowLayered g u v f = Map.adjust (Map.adjust (\c -> c - f) v) u g


-- Assumes there are no cycles of size 2. Inshallah will be fixed later
constructResidual :: Graph -> Graph
constructResidual g = fromEdgeList $ edges ++ map (\(u, v, c) -> (v, u, 0)) edges where
    edges = toEdgeList g


removeEdge :: Graph -> Vertex -> Vertex -> Graph
removeEdge g u v = Map.adjust (Map.delete v) u g

restorePath :: VertexMap Vertex -> Vertex -> Vertex -> Maybe Path
restorePath parents s t
    | s == t = Just []
    | not $ Map.member t parents = Nothing
    | otherwise = case restorePath parents s (parents Map.! t) of
        Nothing -> Nothing
        Just p -> Just (t:p)


-- reads a graph from a file in the format:
-- s t   -- source and sink on the first line
-- u v c -- edge from u to v with capacity c on the remaining lines
readFileEdgeList :: String -> IO (Vertex, Vertex, [Edge])
readFileEdgeList filename = do
        handle <- openFile filename ReadMode
        contents <- hGetContents handle
        let ls = words contents
            [s, t] = map read $ take 2 ls
            edges = map (\(u, v, c) -> (read u, read v, read c)) . group3 $ drop 2 ls
        return (s, t, edges)
        where group3 [] = []
              group3 (x:y:z:xs) = (x, y, z) : group3 xs
