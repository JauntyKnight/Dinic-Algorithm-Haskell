-- Dinic's Algorithm

module Dinic where

import FlowGraph

import Data.Maybe
import Data.Array as Array (array, bounds) -- for a Graph implementation
import qualified Data.Sequence as Seq (Seq, null, singleton, (|>), index, drop) -- for a Queue implementation
import qualified Data.IntMap.Strict as Map (IntMap, singleton, null, member, insert, assocs, (!), fromList, empty) -- for a Dictionary implementation
import Debug.Trace

type LayeredStructure = Map.IntMap Int

-- Assign layers to the vertices of the graph in a bfs manner
-- Returns: a dictionary mapping each vertex to its layer
layers :: Graph -> Vertex -> LayeredStructure
layers g s = layers' g s (Seq.singleton (s, 0)) (Map.singleton s 0) where
    layers' g s q m
        | Seq.null q = m                    -- no more vertices to visit, base case
        | otherwise = layers' g s q'' m'' where    -- recursive case: take the top of the queue, visit its neighbors, and add them to the queue
            (v, d) = Seq.index q 0          -- get the top of the queue
            q' = Seq.drop 1 q               -- remove the top of the queue
            -- add the neighbors of v to the queue if they are not already in the dictionary
            (q'', m'') = foldl (\(q, m) (u, c) -> if Map.member u m || c <= 0 then (q, m) else (q Seq.|> (u, d + 1), Map.insert u (d+1) m)) (q', m) (neighbors g v)

-- Construct the layered graph
-- i.e. keep only the edges that go from a vertex of layer i to a vertex of layer i + 1
-- Complexity: O(V + E)
layeredGraph :: Graph -> Vertex -> Vertex -> LayeredStructure -> Graph
layeredGraph g s t layers_ = Map.fromList [(v, Map.fromList $ filter (\(u, c) -> Map.member u layers_  && (layers_ Map.! u == d + 1)) (neighbors g v)) | (v, d) <- Map.assocs layers_, d <= max_depth]
    where max_depth = layers_ Map.! t

-- Find an augmenting path in the layered graph in a DFS manner
-- Returns: a path from s to t in the layered graph
findBlocking :: Graph -> LayeredStructure -> Vertex -> Vertex -> Maybe Path
findBlocking g layers_ s t = reverse <$> restorePath (findAugmenting' g layers_ t [s] Map.empty) s t where
        max_depth = layers_ Map.! t
        findAugmenting' g layers_ t stack parents
            | null stack = parents
            | s == t = parents
            | layers_ Map.! s >= max_depth = findAugmenting' g layers_ t (tail stack) parents  -- no need to explore this vertex
            | otherwise = findAugmenting' g layers_ t stack' parents' where
                s = head stack
                nbs = [v | (v, c) <- neighbors g s, c > 0, not $ Map.member v parents]
                stack' = nbs ++ tail stack
                parents' = foldl (\parents v -> Map.insert v s parents) parents nbs


pushAugmenting :: Graph -> Path -> (Graph -> Vertex -> Vertex -> Flow -> Graph) -> (Flow, Graph)
pushAugmenting g p pushFlow_ = (f, foldl (\g (u, v) -> pushFlow_ g u v f) g edges) where
    edges = zip p (tail p)
    f = minimum [c | (u, v) <- edges, let (_, _, c) = getEdge g u v]


pushWhilePossible :: Graph -> Graph -> LayeredStructure -> Vertex -> Vertex -> (Flow, Graph, Graph)
pushWhilePossible g layered_g layers_ s t = case augmenting_path of
    Nothing -> (0, g, layered_g)
    Just p -> (f + f'', g'', layered_g'')
    where augmenting_path = findBlocking layered_g layers_ s t
          Just path = augmenting_path
          (f, g') = pushAugmenting g path pushFlow
          (_, layered_g') = pushAugmenting layered_g path pushFlowLayered
          (f'', g'', layered_g'') = pushWhilePossible g' layered_g' layers_ s t


-- | Dinic Algorithm
--   Complexity: O(V^2 E)
dinic :: Graph -> Vertex -> Vertex -> (Flow, Graph)
dinic g s t
    | Map.null layers_ || not (Map.member t layers_) = (0, g)  -- t is not reachable from s
    | otherwise = (f' + f'', g'')
    where layers_ = layers g s
          layered_g = layeredGraph g s t layers_
          (f', g', _) = pushWhilePossible layered_g layered_g layers_ s t
          (f'', g'') = dinic g' s t



