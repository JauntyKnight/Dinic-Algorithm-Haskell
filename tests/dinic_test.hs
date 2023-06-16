import FlowGraph
import Dinic
import Debug.Trace


main = do
    (s, t, es) <- readFileEdgeList "basic_network.txt"
    let g = fromEdgeList es
    let (f, edges) = dinicWithEdgesFlow g es s t
    print f
    print edges
