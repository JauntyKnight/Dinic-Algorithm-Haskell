import FlowGraph
import Dinic
import Debug.Trace


main = do
    (s, t, es) <- readFileEdgeList "basic_network.txt"
    let g = trace (show $ fromEdgeList es) fromEdgeList es
    let (f, g') = dinic g s t
    print f
