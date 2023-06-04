import FlowGraph
import Dinic
import Debug.Trace


main = do
    (s, t, es) <- readFileEdgeList "network.txt"
    let g = fromEdgeList es
    let (f, g') = dinic g s t
    print f
