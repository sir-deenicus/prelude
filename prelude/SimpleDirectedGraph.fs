module Prelude.SimpleDirectedGraphs

open Prelude.Common
open System
open Prelude.Collections.FibonacciHeap
open Prelude.SimpleGraphs
open Microsoft.Collections.Extensions
 
type LabeledDirectedGraph<'a, 'b when 'a : equality and 'a : comparison>() =
    let mutable edges = Dict<'a, Hashset<'a>>()
    let mutable edgeWeights =
        Dict<struct ('a * 'a), 'b>(HashIdentity.Structural)

    member __.Clear() =
        edges.Clear()
        edgeWeights.Clear()

    member g.EdgeData = edges

    member g.InsertRange(es, ews) =
        edges.Clear()
        edges <- es
        edgeWeights.Clear()
        edgeWeights <- ews

    member g.InsertVertex(s : 'a) =
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s, Hashset())
        contained

    member g.InEdges v =
        [| for (KeyValue(struct (a, b), _)) in edgeWeights do
               if b = v then yield (a, b) |]

    member g.Remove(v : 'a) =
        match (edges.tryFind v) with
        | None -> false
        | Some elist ->
            for vert2 in elist do
                let node = struct (v, vert2)
                match edgeWeights.tryFind node with
                | Some _ -> ignore <| edgeWeights.Remove node
                | _ -> ()
            let removes = ResizeArray()
            for (KeyValue(struct (a, b) as node, _)) in edgeWeights do
                if b = v then
                    edges.[a].Remove(b) |> ignore
                    removes.Add(node)
                    ()
            for n in removes do
                edgeWeights.Remove n |> ignore
            edges.Remove v

    member g.InsertEdge(v0, v1, w) =
        maybe {
            let! elist0 = edges.tryFind v0
            let added1 = elist0.Add(v1)
            ignore <| edgeWeights.ExpandElseAdd (struct (v0, v1)) id w
            return (added1)
        }

    member g.AdjustEdge f v1 v2 =
        match edgeWeights.tryFind (struct (v1, v2)) with
        | None -> ()
        | Some e -> edgeWeights.[struct (v1, v2)] <- f e

    member g.ContainsVertex v = edges.ContainsKey v
    member __.EdgeWeights = edgeWeights
    member g.GetEdge v1 v2 = maybe { let! w = edgeWeights.tryFind
                                                  (struct (v1, v2))
                                     return w }
    member g.ContainsEdge v1 v2 = maybe { let! elist0 = edges.tryFind v1
                                          return (elist0.Contains v2) }
    member g.GetEdges v = maybe { let! elist = edges.tryFind v
                                  return elist }

    member g.Edges =
        Hashset(g.EdgeData
                |> Seq.collect (fun (DictKV(v1, vs)) ->
                       vs
                       |> Seq.map
                              (fun v2 -> (v1, v2), edgeWeights.[struct (v1, v2)])
                       |> Seq.toArray)
                |> Seq.toArray)
        |> Seq.toArray

    member g.Vertices = Hashset(edges.Keys)

type WeightedDirectedGraph<'a when 'a : equality and 'a : comparison>(?trackweights) =
    let mutable edges = Dict<'a, Hashset<WeightedNode<'a>>>()
    let mutable edgeWeights =
        Dict<struct ('a * 'a), float>(HashIdentity.Structural)
    let track_weights = defaultArg trackweights false

    member __.Clear() =
        edges.Clear()
        edgeWeights.Clear()

    member g.EdgeData = edges

    member g.InsertRange(es, ews) =
        edges.Clear()
        edges <- es
        edgeWeights.Clear()
        edgeWeights <- ews

    member g.InsertVertex(s : 'a) =
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s, Hashset())
        contained

    member g.InEdges v =
        [| for (KeyValue(struct (a, b), _)) in edgeWeights do
               if b = v then yield (a, b) |]

    member g.Remove(v : 'a) =
        match (edges.tryFind v) with
        | None -> false
        | Some elist ->
            for weightednode in elist do
                let vert1, vert2 = (v, weightednode.Node)
                let node = struct (vert1, vert2)
                match edgeWeights.tryFind node with
                | Some _ -> ignore <| edgeWeights.Remove node
                | _ -> ()
            let removes = ResizeArray()
            for (KeyValue(struct (a, b) as node, w)) in edgeWeights do
                if b = v then
                    edges.[a].Remove(WeightedNode(w, b)) |> ignore
                    removes.Add(node)
                    ()
            for n in removes do
                edgeWeights.Remove n |> ignore
            edges.Remove v

    member g.InsertEdge(v0, v1, w) =
        if not track_weights || not <| edgeWeights.ContainsKey(struct (v0, v1)) then
            maybe {
                let! elist0 = edges.tryFind v0
                let added1 = elist0.Add(WeightedNode(w, v1))

                let _ =
                    if track_weights then
                        ignore
                        <| edgeWeights.ExpandElseAdd (struct (v0, v1)) id w
                    else ()
                return (added1)
            }
        else None

    member g.ContainsVertex v = edges.ContainsKey v

    member g.AdjustWeight f v1 v2 =
        if track_weights then
            maybe {
                let! w = match edgeWeights.tryFind (struct (v1, v2)) with
                         | Some _ as w -> w
                         | _ ->
                             g.InsertEdge(v1, v2, 0.) |> ignore
                             Some 0.
                let v' = WeightedNode(w, v2)
                let elist0 = edges.[v1]
                elist0.Remove(v') |> ignore
                let w' = f w
                let adj1 = elist0.Add(WeightedNode(w', v2))
                edgeWeights.[struct (v1, v2)] <- w'
                return (adj1)
            }
        else None

    member __.EdgeWeights = edgeWeights
    member g.GetEdgeWeight v1 v2 = maybe { let! w = edgeWeights.tryFind
                                                        (struct (v1, v2))
                                           return w }
    member g.ContainsEdge v1 v2 = maybe { let! elist0 = edges.tryFind v1
                                          return (elist0.Contains v2) }
    member g.GetEdges v = maybe { let! elist = edges.tryFind v
                                  return elist }

    member g.Edges =
        Hashset(g.EdgeData
                |> Seq.collect (fun (DictKV(k, v)) ->
                       v
                       |> Seq.map (fun w_v2 -> (k, w_v2.Node), w_v2.Weight)
                       |> Seq.toArray)
                |> Seq.toArray)
        |> Seq.toArray

    member g.OrderedEdges =
        let sorted = Collections.Generic.SortedSet()
        g.EdgeData
        |> Seq.iter
               (fun (DictKV(k, v)) ->
               v
               |> Seq.iter
                      (fun w_v2 ->
                      sorted.Add(WeightedNode(w_v2.Weight, (k, w_v2.Node)))
                      |> ignore))
        sorted

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
            recurse (fun _ -> currentCut.Count = 0) (fun (v, i, getnodes) ->
                if getnodes then
                    for wnode in edges.[v] do
                        if currentCut.Contains wnode.Node
                           || currentCut.Contains v then
                            ignore
                            <| fs.Add
                                   (WeightPair
                                        (wnode.Weight * dir, v, wnode.Node))
                            ()
                if fs.Count = 0 then (currentCut |> Seq.head, i + 1, true)
                else
                    let v0, v2, w, next =
                        let minel = fs.Min
                        minel.VertX, minel.VertY, minel.Weight, minel

                    let _ = fs.Remove next
                    if (currentCut.Contains v0 || currentCut.Contains v2) then
                        let vin0 = tree.InsertVertex v0
                        let vin = tree.InsertVertex v2
                        let nedge = tree.InsertEdge(v0, v2, w)
                        let _ = currentCut.Remove v0
                        let _ = currentCut.Remove v2
                        (v2, i + 1, true)
                    else (v, i + 1, false)) (root, 0, true)
        tree

    member g.Vertices = Hashset(edges.Keys)

    member g.dijkstra_full source =
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
            let adjs = g.GetEdges next
            let _ = visited.Add next
            match adjs with
            | None -> false
            | Some nodes ->
                for weightednode in nodes do
                    if not (visited.Contains weightednode.Node) then
                        let alt = dists.[next] + weightednode.Weight
                        if alt < dists.[weightednode.Node] then
                            dists.[weightednode.Node] <- alt
                            prev.[weightednode.Node] <- Some next
                            FibHeap.decrease_key q nodeMap.[weightednode.Node] alt
                false) false
        |> ignore
        dists, prev

    member g.shortest_dists (dists : Dict<_, _>) (prev : Dict<_, _>) target =
        recurse (fst >> Option.isNone)
            (fun ((Some p), l) -> prev.getOrDefault None p, (p, dists.[p]) :: l)
            (Some target, [])
    member g.shortest_path (prev : Dict<_, _>) target =
        recurse (fst >> Option.isNone)
            (fun ((Some p), l) -> prev.getOrDefault None p, p :: l)
            (Some target, [])
    member g.dijkstra source target =
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
            if next = target then true
            else
                let adjs = g.GetEdges next
                let _ = visited.Add next
                match adjs with
                | None -> false
                | Some vs ->
                    for wnode in vs do
                        if not (visited.Contains wnode.Node) then
                            let alt = dists.[next] + wnode.Weight
                            if alt < dists.[wnode.Node] then
                                dists.[wnode.Node] <- alt
                                prev.[wnode.Node] <- Some next
                                FibHeap.decrease_key q nodeMap.[wnode.Node] alt
                    false) (false)
        |> ignore
        recurse (fst >> Option.isNone)
            (fun ((Some p), l) -> prev.getOrDefault None p, p :: l)
            (Some target, [])

//
///Can have any edge type
type WeightedDirectedGraphCompressed<'a, 'b when 'a : equality and 'a : comparison and 'a :> IEquatable<'a>>() =
    let mutable edges = DictionarySlim<int, DictionarySlim<int, 'b>>()
    let vertices = DictionarySlim<'a, int>()
    let rev_vertices = DictionarySlim<int, 'a>()
    member g.EdgeData = edges
    member __.GraphData f =
        (keyValueSeqtoPairArray edges
         |> Array.map (keepLeft (keyValueSeqtoPairArray >> Array.map (keepLeft f))),
         keyValueSeqtoPairArray vertices) 

    member g.InsertRange fn (es, vs) =
        edges.Clear()
        vertices.Clear() 

        let dslim = DictionarySlim()
        for (e, ns) in es do
            let d = DictionarySlim<int,'b>()
            for (e2,v) in ns do            
                let f = &d.GetOrAddValueRef(e2)
                f <- fn v
            let xs = &dslim.GetOrAddValueRef(e)
            xs <- d

        edges <- dslim

        Array.iter (fun (x, i) ->
            let index = &vertices.GetOrAddValueRef x in index <- i) vs

        g.ComputeReverseIndex()

    member __.Clear() =
        edges.Clear()
        vertices.Clear()
        rev_vertices.Clear()

    member g.ComputeReverseIndex() =
        rev_vertices.Clear()
        for (KeyValue(k, i2)) in vertices do
            let item = &rev_vertices.GetOrAddValueRef i2
            item <- k

    member g.InsertVertex(s : 'a) =
        let contained = vertices.ContainsKey s
        if not contained then
            let count = vertices.Count
            let vs = &vertices.GetOrAddValueRef s
            vs <- count
            let d = &edges.GetOrAddValueRef(count)
            d <- DictionarySlim()
        contained

    member g.Remove(v : 'a) =
        let has, i = vertices.TryGetValue v
        if has then
            for (KeyValue(k, es)) in edges do
                if es.ContainsKey i then
                    es.Remove i |> ignore
                    ()
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

    member g.AdjustWeight f v1 v2 =
        let has1, i = vertices.TryGetValue v1
        if has1 then
            let has2, i2 = vertices.TryGetValue v2
            if has2 then
                let _, es = edges.TryGetValue i
                if es.ContainsKey i2 then
                    let w = &es.GetOrAddValueRef i2
                    w <- f w

    member g.GetEdgeWeight v1 v2 =
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

    member g.ContainsEdge v1 v2 =
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
        Hashset(g.EdgeData
                |> Seq.collect (fun (DictKV(k, v)) ->
                       v
                       |> Seq.map
                              (fun (KeyValue(k2, w)) ->
                              (rev_vertices.TryGetValue k |> snd,
                               rev_vertices.TryGetValue k2 |> snd), w)
                       |> Seq.toArray)
                |> Seq.toArray)
        |> Seq.toArray

    member g.Vertices =
        [| for kv in vertices -> kv.Key |]

type WeightedDirectedGraphBasic<'a, 'b when 'a : equality and 'a : comparison>() =
    let mutable edges = Dict<'a, Dict<'a, 'b>>()
    member g.EdgeData = edges

    member g.InsertRange es =
        edges.Clear()
        edges <- es

    member __.Clear() = edges.Clear()

    member g.InsertVertex(s : 'a) =
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s, Dict())
        contained

    member g.Remove(v : 'a) =
        match (edges.tryFind v) with
        | None -> false
        | Some _ ->
            for (KeyValue(k, es)) in edges do
                if es.ContainsKey v then
                    es.Remove v |> ignore
                    ()
            edges.Remove v

    member g.InsertEdge(v0, v1, w) =
        match edges.tryFind v0 with
        | None -> ()
        | Some es -> es.ExpandElseAdd v1 (fun _ -> w) w

    member g.ContainsVertex v = edges.ContainsKey v

    member g.AdjustWeight f v1 v2 =
        maybe {
            let! es = edges.tryFind v1
            let! old = es.tryFind v2
            es.[v2] <- f old
            return true
        }

    member g.GetEdgeWeight v1 v2 = maybe { let! es = edges.tryFind v1
                                           let! w = es.tryFind v2
                                           return w }
    member g.ContainsEdge v1 v2 = maybe { let! elist0 = edges.tryFind v1
                                          return (elist0.ContainsKey v2) }
    member g.GetEdges v = maybe { let! elist = edges.tryFind v
                                  return (keyValueSeqtoPairArray elist) }

    member g.Edges =
        Hashset(g.EdgeData
                |> Seq.collect (fun (DictKV(k, v)) ->
                       v
                       |> Seq.map (fun (KeyValue(k2, w)) -> (k, k2), w)
                       |> Seq.toArray)
                |> Seq.toArray)
        |> Seq.toArray

    member g.Vertices = Hashset(edges.Keys)


