# Dinic Algorithm for Maximum Flow in Haskell

The repository provides a Haskell implementation of the Dinic algorithm for finding the maximum flow of in a given graph. It runs in `O(V^2 * E)` time complexity, where `V` is the number of vertices and `E` is the number of edges in the graph. To my knowledge, there is no other implementation of Dinic's algorithm in Haskell that would achieve this complexity, because of the difficulties imposed by functional languages, i.e. immutable states.


Functional languages aren't well suited for flow algorithms, as all of them are consistenlty trying to improve an already present flow, and thus immutable data structures are usually the bottleneck for flow algorithms in functional languages. This implementation is trying to find a workaround by using `Data.IntMap` for storing the state of the graph, which allow for essentially constant time updates of the flow.


## Usage

First, you need to construct the graph. A special type, `FlowGraph` is required. We offer a helper function `fromAdjacencyList` that constructs the graph from an adjacency list. 

Then, you can use the `dinic` function to simply get the value of the maximum flow, or `dinicWithEdgesFlow` to get the flow on each edge.

```haskell
import FlowGraph
import Dinic
import Debug.Trace


main = do
    (s, t, es) <- readFileEdgeList "basic_network.txt"
    let g = fromEdgeList es
    let (f, edges) = dinicWithEdgesFlow g es s t
    print f
    print edges
```

