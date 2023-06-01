-- Dinic Algorithm

module Dinic where

import FlowGraph

import Data.Maybe
import Data.Array as Array (array, bounds) -- for a Graph implementation
import Data.Sequence as Sequence (Seq, null, singleton, (|>), index, drop) -- for a Queue implementation
import qualified Data.IntMap.Strict as Map (IntMap, singleton, null, member, insert, assocs, (!), fromList, empty) -- for a Dictionary implementation

type LayeredStructure = Map.IntMap Int

-- Assign layers to the vertices of the graph in a bfs manner
-- Returns: a dictionary mapping each vertex to its layer
layers :: Graph -> Vertex -> Vertex -> LayeredStructure
layers g s t = layers' g s t (Sequence.singleton (s, 0)) (Map.singleton s 0) where
    layers' g s t q m
        | Sequence.null q = m                    -- no more vertices to visit, base case
        | otherwise = layers' g s t q' m' where  -- recursive case: take the top of the queue, visit its neighbors, and add them to the queue
            (v, d) = Sequence.index q 0          -- get the top of the queue
            q' = Sequence.drop 1 q               -- remove the top of the queue
            m' = Map.insert v d m                -- add the vertex to the dictionary
            -- add the neighbors of v to the queue if they are not already in the dictionary
            q'' = foldl (\q (u, (c, f)) -> if Map.member u m' && f < c then q else q |> (u, d + 1)) q' (neighbors g v)

-- Construct the layered graph
-- i.e. keep only the edges that go from a vertex of layer i to a vertex of layer i + 1
-- Complexity: O(V + E)
layeredGraph :: Graph -> Vertex -> Vertex -> LayeredStructure -> Graph
layeredGraph g s t layers = Map.fromList [(v, Map.fromList $ filter (\(u, (c, f)) -> Map.member u layers && (layers Map.! u == d + 1)) (neighbors g v)) | (v, d) <- Map.assocs layers]

-- Find an augmenting path in the layered graph in a DFS manner
-- Returns: a path from s to t in the layered graph
findAugmenting :: Graph -> LayeredStructure -> Vertex -> Vertex -> Maybe Path
findAugmenting g layers s t = restorePath (findAugmenting' g layers t [s] Map.empty) s t where
        max_depth = layers Map.! t
        findAugmenting' g layers t stack parents
            | s == t = parents
            | layers Map.! s >= max_depth = findAugmenting' g layers t (tail stack) parents  -- no need to explore this vertex
            | otherwise = findAugmenting' g layers t stack' parents' where
                s = head stack
                nbs = [v | (v, (c, f)) <- neighbors g s, f < c, not $ Map.member v parents]
                stack' = nbs ++ tail stack
                parents' = foldl (\parents v -> Map.insert v s parents) parents nbs


-- | Dinic Algorithm
--   Complexity: O(V^2 E)
dinic :: Graph -> Vertex -> Vertex -> (Flow, Graph)
dinic g s t = (0, g) -- TODO
