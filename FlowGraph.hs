module FlowGraph where

import Data.Array as Array
import qualified Data.IntMap.Strict as Map

type Vertex = Int
type Capacity = Float
type Flow = Float
type Edge = (Vertex, Vertex, Capacity, Flow)
type Graph = Array Vertex (Map.IntMap (Capacity, Flow))


neighbors :: Graph -> Vertex -> [(Vertex, (Capacity, Flow))]
neighbors g v = Map.assocs (g ! v)
