-- Dinic Algorithm

module Dinic where

import FlowGraph

import Data.Array as Array (array, bounds) -- for a Graph implementation
import Data.Sequence as Sequence (Seq, null, singleton, (|>), index, drop) -- for a Queue implementation
import qualified Data.IntMap.Strict as Map (IntMap, singleton, null, member, insert, assocs, (!), fromList) -- for a Dictionary implementation


-- A path is a sequence of edges
type Path = [Edge]

-- Assign layers to the vertices of the graph in a bfs manner
-- Returns: a dictionary mapping each vertex to its layer
layers :: Graph -> Vertex -> Vertex -> Map.IntMap Int
layers g s t = layers' g s t (Sequence.singleton (s, 0)) (Map.singleton s 0) where
    layers' g s t q m
        | Sequence.null q = m                    -- no more vertices to visit, base case
        | otherwise = layers' g s t q' m' where  -- recursive case: take the top of the queue, visit its neighbors, and add them to the queue
            (v, d) = Sequence.index q 0          -- get the top of the queue
            q' = Sequence.drop 1 q               -- remove the top of the queue
            m' = Map.insert v d m                -- add the vertex to the dictionary
            -- add the neighbors of v to the queue if they are not already in the dictionary
            q'' = foldl (\q (u, (c, f)) -> if Map.member u m' then q else q |> (u, d + 1)) q' (neighbors g v)


-- Construct the layered graph
-- i.e. keep only the edges that go from a vertex of layer i to a vertex of layer i + 1
-- Complexity: O(V + E)
layeredGraph :: Graph -> Vertex -> Vertex -> Map.IntMap Int -> Graph
layeredGraph g s t layers = array (bounds g) [(v, Map.fromList $ filter (\(u, (c, f)) -> Map.member u layers && (layers Map.! u == d + 1)) (neighbors g v)) | (v, d) <- Map.assocs layers]

-- Find the augmenting paths in the layered graph
-- Complexity: O(V)
-- augmentingPath :: Graph -> Vertex -> Vertex -> Graph -> [Vertex]
-- augmentingPath g s t lg = augmentingPath' g s t lg [s] where
--     augmentingPath' g s t lg p
--         | null p = [] -- no path found
--         | v == t = [p]
--         | otherwise = augmentingPath' g s t lg (e:p) where
--             v = head p  -- the current vertex
--             e = head (neighbors lg v)



-- | Dinic Algorithm
--   Complexity: O(V^2 E)
dinic :: Graph -> Vertex -> Vertex -> (Flow, Graph)
dinic g s t = (0, g) -- TODO
