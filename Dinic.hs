-- Dinic Algorithm

module Dinic where

import FlowGraph

import Data.Array
import Data.Sequence as Sequence (Seq, null, singleton, (|>), index, drop) -- for a Queue implementation
import Data.IntMap.Strict as Map (IntMap, singleton, null, member, insert) -- for a Dictionary implementation


-- Construct the layered graph in a bfs manner
-- Complexity: O((V + E)logV), in practice O(V + E)
layers :: Graph -> Vertex -> Vertex -> IntMap Int
layers g s t = layers' g s t (Sequence.singleton (s, 0)) (Map.singleton s 0) where
    layers' g s t q m
        | Sequence.null q = m                    -- no more vertices to visit, base case
        | otherwise = layers' g s t q' m' where  -- recursive case: take the top of the queue, visit its neighbors, and add them to the queue
            (v, d) = Sequence.index q 0
            q' = Sequence.drop 1 q
            m' = Map.insert v d m
            q'' = foldl (\q (u, c, f) -> if Map.member u m' then q else q |> (u, d + 1)) q' (neighbors g v)


-- | Dinic Algorithm
--   Complexity: O(V^2 E)
dinic :: Graph -> Vertex -> Vertex -> (Flow, Graph)

