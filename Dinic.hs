-- Dinic's Algorithm

module Dinic where

import FlowGraph

import Data.Maybe
import Data.Array as Array (array, bounds) -- for a Graph implementation
import Data.Sequence as Sequence (Seq, null, singleton, (|>), index, drop) -- for a Queue implementation
import qualified Data.IntMap.Strict as Map (IntMap, singleton, null, member, insert, assocs, (!), fromList, empty) -- for a Dictionary implementation

type LayeredStructure = Map.IntMap Int

-- Assign layers to the vertices of the graph in a bfs manner
-- Returns: a dictionary mapping each vertex to its layer
layers :: Graph -> Vertex -> LayeredStructure
layers g s = layers' g s (Sequence.singleton (s, 0)) (Map.singleton s 0) where
    layers' g s q m
        | Sequence.null q = m                    -- no more vertices to visit, base case
        | otherwise = layers' g s q' m' where    -- recursive case: take the top of the queue, visit its neighbors, and add them to the queue
            (v, d) = Sequence.index q 0          -- get the top of the queue
            q' = Sequence.drop 1 q               -- remove the top of the queue
            m' = Map.insert v d m                -- add the vertex to the dictionary
            -- add the neighbors of v to the queue if they are not already in the dictionary
            q'' = foldl (\q (u, c) -> if Map.member u m' && c > 0 then q else q |> (u, d + 1)) q' (neighbors g v)

-- Construct the layered graph
-- i.e. keep only the edges that go from a vertex of layer i to a vertex of layer i + 1
-- Complexity: O(V + E)
layeredGraph :: Graph -> Vertex -> Vertex -> LayeredStructure -> Graph
layeredGraph g s t layers_ = Map.fromList [(v, Map.fromList $ filter (\(u, c) -> Map.member u layers_  && (layers_ Map.! u == d + 1)) (neighbors g v)) | (v, d) <- Map.assocs layers_, d <= max_depth]
    where max_depth = layers_ Map.! t

-- Find an augmenting path in the layered graph in a DFS manner
-- Returns: a path from s to t in the layered graph
findBlocking :: Graph -> LayeredStructure -> Vertex -> Vertex -> Maybe Path
findBlocking g layers s t = restorePath (findAugmenting' g layers t [s] Map.empty) s t where
        max_depth = layers Map.! t
        findAugmenting' g layers t stack parents
            | s == t = parents
            | layers Map.! s >= max_depth = findAugmenting' g layers t (tail stack) parents  -- no need to explore this vertex
            | otherwise = findAugmenting' g layers t stack' parents' where
                s = head stack
                nbs = [v | (v, c) <- neighbors g s, c > 0, not $ Map.member v parents]
                stack' = nbs ++ tail stack
                parents' = foldl (\parents v -> Map.insert v s parents) parents nbs


pushAugmenting :: Graph -> Path -> (Flow, Graph)
pushAugmenting g p = (f, foldl (\g (u, v) -> pushFlow g u v f) g edges) where
    edges = zip p (tail p)
    f = minimum [c | (u, v) <- edges, let (_, _, c) = getEdge g u v]


pushWhilePossible :: Graph -> Vertex -> Vertex -> LayeredStructure -> (Flow, Graph)
pushWhilePossible g s t layers_ = case augmenting_path of
    Nothing -> (0, g)
    Just p -> (f + f'', g'')
    where augmenting_path = findBlocking g layers_ s t
          Just path = augmenting_path
          (f, g') = pushAugmenting g path
          (f'', g'') = pushWhilePossible g' s t layers_


-- | Dinic Algorithm
--   Complexity: O(V^2 E)
dinic :: Graph -> Vertex -> Vertex -> (Flow, Graph)
dinic g s t
    | Map.null layers_ || not (Map.member t layers_) = (0, g)  -- t is not reachable from s
    | otherwise = (f' + f'', g'')
    where layers_ = layers g s
          layered_g = layeredGraph g s t layers_
          (f', g') = pushWhilePossible layered_g s t layers_
          (f'', g'') = dinic g' s t
