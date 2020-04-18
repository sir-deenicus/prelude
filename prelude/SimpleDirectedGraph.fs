module Prelude.SimpleDirectedGraphs

open Prelude.Common
open System
open Prelude.Collections.FibonacciHeap
open Prelude.SimpleGraphs
open Microsoft.Collections.Extensions

///Fastweights is better for lots of look ups and adjusting of weights, otherwise overhead is not worth it.
type WeightedDirectedGraph<'a when 'a : equality and 'a : comparison>(?fastweights) =
    let mutable edges = Dict<'a,Dict<'a, float>>()
    let mutable edgeWeights = 
        Dict<struct ('a * 'a), float>(HashIdentity.Structural)
    let fast_weights = defaultArg fastweights false

    member __.Clear() =
        edges.Clear()
        edgeWeights.Clear()

    member g.EdgeData = edges

    member g.ForEachEdge f =
        for KeyValue(v, vs) in edges do
            for KeyValue(v2, w) in vs do
                edges.[v].[v2] <- f w

    member g.RemoveVerticesWhere f =
        let keys = g.Vertices
        for k in keys do
            if f k then
                g.Remove k |> ignore
                 
    member g.RemoveEdgesWhere f =
        for (v, vs) in keyValueSeqtoPairArray edges do
            for (v2, w) in keyValueSeqtoPairArray vs do
                if not (f ((v,v2),w)) then g.RemoveEdge(v,v2) |> ignore

    member g.InsertRange(edgesRaw,?edgeweights) =
        edges.Clear()
        edges <- edgesRaw
        edgeWeights.Clear()
        if edgeweights.IsSome then
           edgeWeights <- edgeweights.Value

    member g.InsertVertex(s : 'a) =
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s, Dict())
        contained

    member g.InEdges v =
        if fast_weights then
            [| for (KeyValue(struct (a, b), _)) in edgeWeights do
                   if b = v then yield (a, b) |]
        else 
            [|for KeyValue(a, vs) in edges do
                for KeyValue(b, _) in vs do
                       if b = v then yield (a, b) |]

    member g.Remove(v : 'a) =
        match (edges.tryFind v) with
        | None -> false
        | Some edgelist ->
            if fast_weights then 
                for KeyValue(connectedNode,_) in edgelist do 
                    let node = struct (v,connectedNode)
                    match edgeWeights.tryFind node with
                    | Some _ -> ignore (edgeWeights.Remove node)
                    | _ -> ()  
     
            let removes = g.InEdges v 
            for (a,b) in removes do //all a's pointing into v=b
                edges.[a].Remove b |> ignore
                if fast_weights then
                    let n = struct (a,b)
                    edgeWeights.Remove(n) |> ignore
            edges.Remove v 

    member g.InsertEdge(node1,node2,w) = 
        if not fast_weights || not(edgeWeights.ContainsKey(struct (node1,node2))) then 
            maybe {
                let! connectedsToNode1 = edges.tryFind node1
                if connectedsToNode1.ContainsKey node2 then 
                   return false
                else
                    connectedsToNode1.Add(node2, w)
                
                    let _ =
                        if fast_weights then 
                            ignore(edgeWeights.ExpandElseAdd (struct (node1,node2)) id w) 
                    return true
            }
        else None

    member g.RemoveEdge(v1,v2, ?clearEmptys) =
        let docleanup = defaultArg clearEmptys true
        maybe {
            let! connectedToNode1 = edges.tryFind v1
            let r = connectedToNode1.Remove(v2)

            let _ =
                if docleanup && connectedToNode1.Count = 0
                    && Array.isEmpty (g.InEdges v1) then edges.Remove v1 |> ignore

            let _ =
                if fast_weights then
                    let node = struct (v1, v2)
                    ignore (edgeWeights.Remove(node))
            return r
        } 

    member g.ContainsVertex v = edges.ContainsKey v

    member g.AdjustWeight f (v1,v2) =
        if fast_weights then 
            maybe {       
                match edgeWeights.tryFind(struct (v1,v2)) with
                | Some w -> 
                    edgeWeights.[struct (v1,v2)] <- f w
                    return true
                | None -> return! g.InsertEdge(v1,v2,f 0.)   
            }
        else  
            match edges.tryFind v1 with
            | Some elistV1 -> 
                match elistV1.tryFind v2 with 
                | Some w ->
                    elistV1.[v2] <- f w 
                    Some true
                | None -> g.InsertEdge(v1,v2, f 0.) 
            | None -> Some (false) 

    member __.EdgeWeights = edgeWeights

    member g.GetEdgeWeight (v1, v2) =  
        maybe {
            if fast_weights then
                let! w = edgeWeights.tryFind(struct (v1,v2))
                return w 
            else
                let! vs = edges.tryFind v1
                let! w = vs.tryFind v2
                return w
        } 

    member g.ContainsEdge (v1, v2) = maybe { 
        let! elist0 = edges.tryFind v1
        return (elist0.ContainsKey v2) }

    member g.GetEdgesRaw v = maybe { 
        let! elist = edges.tryFind v
        return elist }

    member g.GetEdges v = maybe {
        let! elist = edges.tryFind v
        return [|for KeyValue(node,weight) in elist -> node, weight |]}
    
    member g.NodeNeighborCounts() = 
        [|for KeyValue(v1,vs) in g.EdgeData -> v1, vs.Count|]  

    member g.Edges = 
        [|for KeyValue(v1,vs) in g.EdgeData do
            for KeyValue(v2,w) in vs -> (v1,v2), w |]  

    member g.OrderedEdges =
        let sorted = Collections.Generic.SortedSet() 
        for KeyValue(v1,vs) in g.EdgeData do
            for KeyValue(v2,w) in vs do 
                sorted.Add (WeightedNode(w, (v1,v2)))
                |> ignore
        [|for v in sorted -> v.Node, v.Weight|]
          
    ///if do max=true it does maximum spanning tree instead
    member g.MinimumSpanningTree(?domax) =
        let dir =
            if (defaultArg domax false) then -1.
            else 1.

        let currentCut = Hashset(edges.Keys)
        let root = currentCut |> Seq.head
        let tree = WeightedDirectedGraph()
        let fs = Collections.Generic.SortedSet()

        let _, _, steps =
            recurse (fun _ -> currentCut.Count = 0) (fun (node1, i, getnodes) ->
                if getnodes then
                    for KeyValue(node2, weight) in edges.[node1] do
                        if currentCut.Contains node2
                           || currentCut.Contains node1 then
                                fs.Add (WeightPair(weight * dir, node1, node2)) 
                                |> ignore
                if fs.Count = 0 then (currentCut |> Seq.head, i + 1, true)
                else
                    let v1, v2, w, next =
                        let minel = fs.Min
                        minel.VertX, minel.VertY, minel.Weight, minel

                    let _ = fs.Remove next
                    if (currentCut.Contains v1 || currentCut.Contains v2) then
                        let _ = tree.InsertVertex v1
                        let _ = tree.InsertVertex v2
                        let _ = tree.InsertEdge(v1, v2, w)
                        let _ = currentCut.Remove v1
                        let _ = currentCut.Remove v2
                        (v2, i + 1, true)
                    else (node1, i + 1, false)) (root, 0, true)
        tree

    member g.Vertices = [|for k in edges.Keys -> k|] 

    member g.extractShortestPath f ((dists: Dict<_,_>, prevs: Dict<_,_>), target) =
        recurse (fst >> Option.isNone) 
            (fun (prev,l) ->
                match prev with 
                | Some p -> prevs.getOrDefault None p,f(p,dists.[p]) :: l 
                | _ -> failwith "no path") 
            (Some target,[])
            |> snd

    member g.shortestPaths source = 
        let paths = g.dijkstrasShortestPath source
        g.Vertices 
        |> Array.map (fun v -> g.extractShortestPath id (paths, v))
        
    member g.dijkstrasShortestPath(source, ?target) =
        let dists = Dict.ofSeq [ source, 0. ]
        let prev = Dict()
        let vs = g.Vertices
        let q = FibHeap.create()
        let visited = Hashset()
        let nodeMap = Dict()
        for v in vs do
            if v <> source then dists.Add(v, Double.MaxValue)
            let _ = prev.Add(v, None)
            let n = FibHeap.insert_data q v dists.[v]
            nodeMap.Add(v, n)
        recurse (fun stop -> stop || FibHeap.size q <= 0) (fun _ ->
            let next = FibHeap.extract_min_data q
            if target.IsSome && next = target.Value then true
            else
                let adjs = g.GetEdgesRaw next
                let _ = visited.Add next
                match adjs with
                | None -> false
                | Some vs ->
                    for KeyValue(node2,weight) in vs do
                        if not (visited.Contains node2) then
                            let alt = dists.[next] + weight
                            if alt < dists.[node2] then
                                dists.[node2] <- alt
                                prev.[node2] <- Some next
                                FibHeap.decrease_key q nodeMap.[node2] alt
                    false) (false)
        |> ignore
        dists, prev

    interface IWeightedGraph<'a,float> with
        member g.Vertices = seq g.Vertices
        member g.Edges = Array.map fst g.Edges
        member g.WeightedEdges = g.Edges
        member g.GetEdges n = g.GetEdges n |> Option.map (Array.map fst)
        member g.GetWeightedEdges n = g.GetEdges n
        member g.IsDirected = true
        member g.AddNode v = g.InsertVertex v |> ignore
        member g.InsertWeightedEdge e = g.InsertEdge e |> ignore
        member g.GetNodeNeighborCounts() = g.NodeNeighborCounts()
        member g.GetRawEdges v =  
            g.GetEdgesRaw v 
            |> Option.map (fun d -> d :> IDict<'a,float>) 

///Can have any edge type
type WeightedDirectedGraphCompressed<'a, 'b when 'a : equality and 'a : comparison and 'a :> IEquatable<'a>>() =
    let mutable edges = DictionarySlim<int, DictionarySlim<int, 'b>>()
    let vertices = DictionarySlim<'a, int>()
    let rev_vertices = DictionarySlim<int, 'a>()
    member g.EdgeData = edges

    member __.GraphData f = 
        [|for (node1, nodes) in keyValueSeqtoPairArray edges ->
            node1, 
            [| for (node2, w) in keyValueSeqtoPairArray nodes -> node2 , f w|]|],
        keyValueSeqtoPairArray vertices

    member g.InsertRange fn (es, vs) =
        edges.Clear()
        vertices.Clear() 

        let graph = DictionarySlim()
        for (node1, nodes) in es do
            let neighbors = DictionarySlim<int,_>()
            for (node2,w) in nodes do            
                let weight = &neighbors.GetOrAddValueRef(node2)
                weight <- fn w
            let node1Neighbors = &graph.GetOrAddValueRef(node1)
            node1Neighbors <- neighbors

        edges <- graph

        Array.iter (fun (item, i) ->
            let index = &vertices.GetOrAddValueRef item in index <- i) vs

        g.ComputeReverseIndex()

    member __.Clear() =
        edges.Clear()
        vertices.Clear()
        rev_vertices.Clear()

    member g.ComputeReverseIndex() =
        rev_vertices.Clear()
        for (KeyValue(key, index)) in vertices do
            let item = &rev_vertices.GetOrAddValueRef index
            item <- key

    member g.InsertVertex(s : 'a) =
        let contained = vertices.ContainsKey s
        if not contained then
            let count = vertices.Count
            let index = &vertices.GetOrAddValueRef s
            index <- count
            let graph = &edges.GetOrAddValueRef(count)
            graph <- DictionarySlim()
        contained

    member g.Remove(v : 'a) =
        let has, i = vertices.TryGetValue v
        if has then
            for (KeyValue(_, es)) in edges do
                if es.ContainsKey i then //points to
                    es.Remove i |> ignore 
            edges.Remove i
        else false

    member g.InsertEdge(v0, v1, w) =
        let has1, i = vertices.TryGetValue v0
        if has1 then
            let has2, i2 = vertices.TryGetValue v1
            if has2 then
                let _, es = edges.TryGetValue i
                let w0 = &es.GetOrAddValueRef i2
                w0 <- w
                true
            else false
        else false

    member g.ContainsVertex v = vertices.ContainsKey v

    member g.AdjustWeight f (v1, v2) =
        let has1, i = vertices.TryGetValue v1
        if has1 then
            let has2, i2 = vertices.TryGetValue v2
            if has2 then
                let _, es = edges.TryGetValue i
                if es.ContainsKey i2 then
                    let w = &es.GetOrAddValueRef i2
                    w <- f w

    member g.GetEdgeWeight (v1, v2) =
        let has1, i = vertices.TryGetValue v1
        if has1 then
            let has2, i2 = vertices.TryGetValue v2
            if has2 then
                let _, es = edges.TryGetValue i
                let has3, w = es.TryGetValue i2
                if has3 then Some w
                else None
            else None
        else None

    member g.ContainsEdge (v1, v2) =
        let has1, i = vertices.TryGetValue v1
        if has1 then
            let has2, i2 = vertices.TryGetValue v2
            if has2 then
                let e = edges.TryGetValue i |> snd
                e.ContainsKey i2
            else false
        else false

    member g.GetEdges v =
        let hasv, i = vertices.TryGetValue v
        if hasv then
            let _, es = edges.TryGetValue i
            Some
                [| for (KeyValue(k, w)) in es ->
                       rev_vertices.TryGetValue k |> snd, w |]
        else None

    member g.Edges =
        [|for KeyValue(node1,nodes) in g.EdgeData do
            for KeyValue(node2, w) in nodes ->
                (rev_vertices.TryGetValue node1 |> snd,
                 rev_vertices.TryGetValue node2 |> snd), w|]
    
    member g.NodeNeighborCounts() =                  
        [|for KeyValue(node1,nodes) in g.EdgeData -> rev_vertices.TryGetValue node1 |> snd, nodes.Count|]
    
    member g.GetEdgesRaw v = Option.map dict (g.GetEdges v)

    member g.Vertices =
        [| for kv in vertices -> kv.Key |]

    interface IWeightedGraph<'a,'b> with
        member g.Vertices = seq g.Vertices
        member g.Edges = Array.map fst g.Edges
        member g.WeightedEdges = g.Edges
        member g.GetEdges n = g.GetEdges n |> Option.map (Array.map fst)
        member g.GetWeightedEdges n = g.GetEdges n
        member g.IsDirected = true
        member g.AddNode v = g.InsertVertex v |> ignore
        member g.InsertWeightedEdge e = g.InsertEdge e |> ignore
        member g.GetNodeNeighborCounts() = g.NodeNeighborCounts()
        member g.GetRawEdges v = g.GetEdgesRaw v  

open CollectionSlim

///look up table for inedges, a bit more memory
type WeightedDirectedGraphCompressed2<'a, 'b when 'a : equality and 'a : comparison and 'a :> IEquatable<'a>>() =
    let mutable edges = DictionarySlim<int, DictionarySlim<int, 'b>>()
    let mutable inedges = DictionarySlim<int, SetSlim<int>>()

    let vertices = DictionarySlim<'a, int>()
    let rev_vertices = DictionarySlim<int, 'a>()
    member g.EdgeData = edges

    member __.GraphData f = 
        [|for (node1, nodes) in keyValueSeqtoPairArray edges ->
            node1, 
            [| for (node2, w) in keyValueSeqtoPairArray nodes -> node2 , f w|]|],
        [|for (node1, nodes) in keyValueSeqtoPairArray inedges -> node1, nodes.ToArray id|],
        keyValueSeqtoPairArray vertices

    member g.InsertRange fn (es, ins, vs) =
        edges.Clear()
        vertices.Clear() 
        inedges.Clear() 

        for (i,vs) in ins do
            let inset = SetSlim<int>()
            for k in vs do 
                inset.Add k |> ignore 
            let ins = &inedges.GetOrAddValueRef i
            ins <- inset
             
        let graph = DictionarySlim()
        for (node1, nodes) in es do
            let neighbors = DictionarySlim<int,_>()
            for (node2,w) in nodes do            
                let weight = &neighbors.GetOrAddValueRef(node2)
                weight <- fn w
            let node1Neighbors = &graph.GetOrAddValueRef(node1)
            node1Neighbors <- neighbors

        edges <- graph

        Array.iter (fun (item, i) ->
            let index = &vertices.GetOrAddValueRef item in index <- i) vs

        g.ComputeReverseIndex()

    member __.Clear() =
        edges.Clear()
        vertices.Clear()
        rev_vertices.Clear()

    member g.ComputeReverseIndex() =
        rev_vertices.Clear()
        for (KeyValue(key, index)) in vertices do
            let item = &rev_vertices.GetOrAddValueRef index
            item <- key

    member g.InsertVertex(s : 'a) =
        let contained = vertices.ContainsKey s
        if not contained then
            let count = vertices.Count
            let index = &vertices.GetOrAddValueRef s
            index <- count
            let graph = &edges.GetOrAddValueRef(count)
            graph <- DictionarySlim()
        contained

    member g.Remove(v : 'a) =
        let has, i = vertices.TryGetValue v
        if has then 
            let _, ins = inedges.TryGetValue i
            ins.iter (fun k -> 
                let _, es = edges.TryGetValue k
                es.Remove i |> ignore) 
            edges.Remove i
        else false

    member g.InsertEdge(v0, v1, w) =
        let has1, i = vertices.TryGetValue v0
        if has1 then
            let has2, i2 = vertices.TryGetValue v1
            if has2 then
                let _, es = edges.TryGetValue i
                let _, ins = inedges.TryGetValue i
                let w0 = &es.GetOrAddValueRef i2
                ins.Add i2 |> ignore
                w0 <- w 
                true
            else false
        else false

    member g.ContainsVertex v = vertices.ContainsKey v

    member g.AdjustWeight f (v1, v2) =
        let has1, i = vertices.TryGetValue v1
        if has1 then
            let has2, i2 = vertices.TryGetValue v2
            if has2 then
                let _, es = edges.TryGetValue i
                if es.ContainsKey i2 then
                    let w = &es.GetOrAddValueRef i2
                    w <- f w

    member g.GetEdgeWeight (v1, v2) =
        let has1, i = vertices.TryGetValue v1
        if has1 then
            let has2, i2 = vertices.TryGetValue v2
            if has2 then
                let _, es = edges.TryGetValue i
                let has3, w = es.TryGetValue i2
                if has3 then Some w
                else None
            else None
        else None

    member g.ContainsEdge (v1, v2) =
        let has1, i = vertices.TryGetValue v1
        if has1 then
            let has2, i2 = vertices.TryGetValue v2
            if has2 then
                let e = edges.TryGetValue i |> snd
                e.ContainsKey i2
            else false
        else false

    member g.GetEdges v =
        let hasv, i = vertices.TryGetValue v
        if hasv then
            let _, es = edges.TryGetValue i
            Some
                [| for (KeyValue(k, w)) in es ->
                       rev_vertices.TryGetValue k |> snd, w |]
        else None

    member g.GetEdgesRaw v = Option.map dict (g.GetEdges v)

    member g.InEdges v =
        let hasv, i = vertices.TryGetValue v
        if hasv then
            let _, es = inedges.TryGetValue i
            Some (es.ToArray (rev_vertices.TryGetValue >> snd))                 
        else None

    member g.Edges =
        [|for KeyValue(node1,nodes) in g.EdgeData do
            for KeyValue(node2, w) in nodes ->
                (rev_vertices.TryGetValue node1 |> snd,
                 rev_vertices.TryGetValue node2 |> snd), w|]
    
    member g.NodeNeighborCounts() = 
        [|for KeyValue(node1,nodes) in g.EdgeData ->
            rev_vertices.TryGetValue node1 |> snd, nodes.Count|]
    
    member g.Vertices =
        [| for kv in vertices -> kv.Key |]

    interface IWeightedGraph<'a,'b> with
        member g.Vertices = seq g.Vertices
        member g.Edges = Array.map fst g.Edges
        member g.WeightedEdges = g.Edges
        member g.GetEdges n = g.GetEdges n |> Option.map (Array.map fst)
        member g.GetWeightedEdges n = g.GetEdges n
        member g.IsDirected = true
        member g.AddNode v = g.InsertVertex v |> ignore
        member g.InsertWeightedEdge e = g.InsertEdge e |> ignore
        member g.GetNodeNeighborCounts() = g.NodeNeighborCounts()
        member g.GetRawEdges v = g.GetEdgesRaw v

type GeneralDirectedGraph<'a, 'b when 'a : equality and 'a : comparison>() =
    let mutable edges = Dict<'a, Dict<'a, 'b>>()

    member g.EdgeData = edges

    member g.InsertRange es =
        edges.Clear()
        edges <- es

    member __.Clear() = edges.Clear()

    member g.ForEachEdge f =
        for KeyValue(v, vs) in edges do
            for KeyValue(v2, w) in vs do
                edges.[v].[v2] <- f w

    member g.RemoveVerticesWhere f =
        let keys = g.Vertices
        for k in keys do
            if f k then
                g.Remove k |> ignore
                 
    member g.RemoveEdgesWhere f =
        for (v, vs) in keyValueSeqtoPairArray edges do
            for (v2, w) in keyValueSeqtoPairArray vs do
                if not (f ((v,v2),w)) then g.RemoveEdge(v,v2) |> ignore

    member g.RemoveEdge(v1,v2, ?clearEmptys) =
        let docleanup = defaultArg clearEmptys true
        maybe {
            let! connectedToNode1 = edges.tryFind v1
            let r = connectedToNode1.Remove(v2)

            let _ =
                if docleanup && connectedToNode1.Count = 0
                    && Array.isEmpty (g.InEdges v1) then edges.Remove v1 |> ignore 
            return r
        } 

    member g.InsertVertex(s : 'a) =
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s, Dict())
        contained

    member g.Remove(v : 'a) =
        match (edges.tryFind v) with
        | None -> false
        | Some _ ->
            for (KeyValue(_, es)) in edges do
                if es.ContainsKey v then
                    es.Remove v |> ignore 
            edges.Remove v

    member g.InEdges v =  
        [|for KeyValue(a, vs) in edges do
            for KeyValue(b, _) in vs do
                    if b = v then yield (a, b) |]

    member g.InsertEdge(v0, v1, w) =
        match edges.tryFind v0 with
        | None -> ()
        | Some es -> es.ExpandElseAdd v1 (fun _ -> w) w

    member g.ContainsVertex v = edges.ContainsKey v

    member g.AdjustWeight f (v1, v2) =
        maybe {
            let! es = edges.tryFind v1
            let! oldweight = es.tryFind v2
            es.[v2] <- f oldweight
            return true
        }

    member g.GetEdgeWeight (v1, v2) = maybe { 
        let! es = edges.tryFind v1
        let! w = es.tryFind v2
        return w }

    member g.ContainsEdge (v1, v2) = maybe { 
        let! elist0 = edges.tryFind v1
        return (elist0.ContainsKey v2) }

    member g.GetEdgesRaw v = maybe { 
        let! elist = edges.tryFind v
        return elist }

    member g.GetEdges v = Option.map keyValueSeqtoPairArray (g.GetEdgesRaw v)
    member g.Edges = 
        [|for KeyValue(v1,vs) in g.EdgeData do
            for KeyValue(v2,w) in vs -> (v1,v2), w |]  

    member g.Vertices = [|for k in edges.Keys -> k|]  

    member g.NodeNeighborCounts() = 
        [|for KeyValue(v1,vs) in g.EdgeData -> v1, vs.Count|]  
      
    interface IWeightedGraph<'a,'b> with
        member g.Vertices = seq g.Vertices
        member g.Edges = Array.map fst g.Edges
        member g.WeightedEdges = g.Edges
        member g.GetEdges n = g.GetEdges n |> Option.map (Array.map fst)
        member g.GetWeightedEdges n = g.GetEdges n
        member g.IsDirected = true
        member g.AddNode v = g.InsertVertex v |> ignore
        member g.InsertWeightedEdge e = g.InsertEdge e |> ignore
        member g.GetNodeNeighborCounts() = g.NodeNeighborCounts()
        member g.GetRawEdges v =  
            g.GetEdgesRaw v 
            |> Option.map (fun d -> d :> IDict<'a,'b>) 


type DirectedGraph<'a when 'a : equality and 'a : comparison>() =
    let mutable edges = Dict<'a,Hashset<'a>>() 

    member __.Clear() = edges.Clear() 

    member g.EdgeData = edges 

    member g.toWeightedGraph() =
        let wg = WeightedDirectedGraph()
        [|for KeyValue(u,vs) in edges ->
            u, Dict.ofSeq [|for v in vs -> v, 1.|] |]
        |> Dict.ofSeq
        |> wg.InsertRange  
        wg

    member g.RemoveVerticesWhere f =
        let keys = g.Vertices
        for k in keys do
            if f k then
                g.Remove k |> ignore
                 
    member g.RemoveEdgesWhere f =
        for (v, vs) in keyValueSeqtoPairArray edges do
            for v2 in vs do
                if not (f ((v,v2))) then g.RemoveEdge(v,v2) |> ignore

    member g.InsertRange(edgesRaw) =
        edges.Clear()
        edges <- edgesRaw 

    member g.InsertVertex(s : 'a) =
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s, Hashset())
        contained

    member g.InEdges v = 
        [|for KeyValue(a, vs) in edges do
            for b in vs do
                if b = v then yield (a, b) |]

    member g.Remove(v : 'a) =
        match (edges.tryFind v) with
        | None -> false
        | Some _ ->  
            let removes = g.InEdges v 
            for (a,b) in removes do //all a's pointing into v=b
                edges.[a].Remove b |> ignore 
            edges.Remove v  

    member g.InsertEdge(node1,node2) =  
        maybe {
            let! connectedsToNode1 = edges.tryFind node1
            if connectedsToNode1.Contains node2 then 
                return false
            else
                connectedsToNode1.Add(node2) |> ignore
                return true
        } 

    member g.RemoveEdge(v1,v2, ?clearEmptys) =
        let docleanup = defaultArg clearEmptys true
        maybe {
            let! connectedToNode1 = edges.tryFind v1
            let r = connectedToNode1.Remove(v2)

            let _ =
                if docleanup && connectedToNode1.Count = 0
                    && Array.isEmpty (g.InEdges v1) then edges.Remove v1 |> ignore 
            return r
        } 

    member g.ContainsVertex v = edges.ContainsKey v 

    member g.ContainsEdge (v1, v2) = maybe { 
        let! elist0 = edges.tryFind v1
        return (elist0.Contains v2) }

    member g.GetEdgesRaw v = maybe { 
        let! elist = edges.tryFind v
        return elist }

    member g.GetEdges v = maybe {
        let! elist = edges.tryFind v
        return [|for node in elist -> node |]}

    member g.Edges = 
        [|for KeyValue(v1,vs) in g.EdgeData do
            for v2 in vs -> (v1,v2)|]   

    member g.Vertices = [|for k in edges.Keys -> k|]  

    member g.NodeNeighborCounts() =
        [|for KeyValue(v1,vs) in g.EdgeData -> (v1,vs.Count)|]   

    interface IGraph<'a> with
        member g.Vertices = seq g.Vertices
        member g.Edges = g.Edges 
        member g.GetEdges n = g.GetEdges n  
        member g.IsDirected = true
        member g.AddNode v = g.InsertVertex v |> ignore 
        member g.GetNodeNeighborCounts() = g.NodeNeighborCounts()