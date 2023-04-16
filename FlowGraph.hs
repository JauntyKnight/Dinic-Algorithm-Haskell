module FlowGraph where

import Data.Array

type Vertex = Int
type Capacity = Float
type Flow = Float
type Edge = (Vertex, Vertex, Capacity, Flow)
type Graph = Array Vertex [(Vertex, Capacity, Flow)]


neighbors :: Graph -> Vertex -> [(Vertex, Capacity, Flow)]
neighbors g v = g ! v
