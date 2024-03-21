module Prelude.SimpleDirectedGraphs

open Prelude.Common
open System
open Prelude.Collections.FibonacciHeap
open Prelude.SimpleGraphs
open DictionarySlim
 
type WeightedDirectedGraph<'a when 'a : equality and 'a : comparison>() =
    let mutable edges = Dict<'a,Dict<'a, float>>()
    let mutable cyclic = None

    let mutable weightnormalized = false

    member g.WeightNormalized
        with get () = weightnormalized
        and set wn = weightnormalized <- wn
 
    member g.IsCyclic   
        with get() = cyclic  

    member g.CheckForCycles() =
        match GraphAlgorithms.isCyclic g with
        | NotCyclic -> cyclic <- Some false
        | IsCyclic _ -> cyclic <- Some true 

    member __.Clear() =
        edges.Clear() 

    member g.EdgeData = edges
    
    member g.ForEachEdge f =
        for (v, vs) in keyValueSeqtoPairArray edges do
            for (v2, w) in keyValueSeqtoPairArray vs do
                edges[v][v2] <- f w

    member g.RemoveVerticesWhere f =
        let keys = g.Nodes
        for k in keys do
            if f k then
                g.Remove k |> ignore
                 
    member g.RemoveEdgesWhere f =
        for (v, vs) in keyValueSeqtoPairArray edges do
            for (v2, w) in keyValueSeqtoPairArray vs do
                if f ((v,v2),w) then g.RemoveEdge(v,v2) |> ignore

    member g.FromDictionary(edgesDict) =
        edges.Clear()
        edges <- edgesDict 

    member g.FromDictionary(edgeWeightsDict) =
        edges.Clear()   
        for KeyValue(struct(v1,v2),w) in edgeWeightsDict do
            match edges.TryFind v1 with
            | Some vs -> vs.Add(v2, w) |> ignore
            | None -> edges.Add(v1, Dict.ofSeq [v2, w]) |> ignore 

    member g.AddNode(s : 'a) =
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s, Dict())
        contained

    /// <summary>
    /// Sends a message `msg` to child nodes of node "v" via f, which takes a message, the current node, edge weight and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.
    /// </summary>
    /// <param name="msg">The message to send</param>
    /// <param name="v">The node to send the message from</param>
    /// <param name="f">The function to apply to the message, f takes a message, the current node, edge weight and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.</param>
    member g.SendMessageToChildren (msg:'b, v, f) =
        let neighbors = g.GetEdges v 
        for (node, weight) in neighbors do
            let msg', terminate = f msg (v, node, weight)
            if terminate then () else g.SendMessageToChildren(msg', node, f)
    
    /// <summary>
    /// Asynchronously sends a state `state` to child nodes of node "v" via f, which takes a state, the current node, edge weight and the current targeted node and returns a new state and a boolean indicating whether to terminate the state passing.
    /// </summary>
    /// <param name="state">The state to send</param>
    /// <param name="v">The node to send the state from</param>
    /// <param name="f">The function to apply to the state, f takes a state, the current node, edge weight and the current targeted node and returns a new state and a boolean indicating whether to terminate the state passing.</param>
    member g.SendMessageToChildrenAsync (state:'b, v, f) = 
        async {
            let neighbors = g.GetEdges v 
            for (node, weight) in neighbors do
                let! state', terminate = async { return f state (v, node, weight) }
                if terminate then () 
                else do! g.SendMessageToChildrenAsync(state', node, f)
        }
        
    /// <summary>
    /// Sends a message `msg` to parent nodes of node "v" via f, which takes a message, the current node, edge weight and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.
    /// </summary>
    /// <param name="msg">The message to send</param>
    /// <param name="v">The node to send the message from</param>
    /// <param name="f">The function to apply to the message, f takes a message, the current node, edge weight and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.</param>
    member g.SendMessageToParents (msg:'b, v, f) =
        let nodes = g.InEdgesWeights v 
        for ((node,_), weight) in nodes do
            let msg', terminate = f msg (v, node, weight)
            if terminate then () else g.SendMessageToParents(msg', node, f)

    /// <summary>
    /// Asynchronously sends a message `msg` to parent nodes of node "v" via f, which takes a message, the current node, edge weight and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.
    /// </summary>
    /// <param name="msg">The message to send</param>
    /// <param name="v">The node to send the message from</param>
    /// <param name="f">The function to apply to the message, f takes a message, the current node, edge weight and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.</param>
    member g.SendMessageToParentsAsync (msg:'b, v, f) =
        async {
            let nodes = g.InEdgesWeights v 
            for ((node,_), weight) in nodes do
                let! msg', terminate = async { return f msg (v, node, weight) }
                if terminate then () 
                else do! g.SendMessageToParentsAsync(msg', node, f)
        }
    
    /// <summary>
    /// Sends a message `state` to neighbors of node "v" via f, which takes a message, the current node and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.
    /// </summary>
    /// <param name="state">The initial state or message to be passed to the neighbors of node "v". This state can be updated with each recursive call based on the return value of function "f".</param>
    /// <param name="v">The node to send the message from</param>
    /// <param name="f">The function to apply to the message, f takes a message, the current node and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.</param>
    member g.SendMessageToNeighbors (state:'b, v, f) =
        let nodes =  
            Array.append (g.GetEdges v) [|for ((u,_), w) in g.InEdgesWeights v -> u, w|]
            
        for (node, weight) in nodes do
            let msg', terminate = f state (v, node, weight)
            if terminate then () else g.SendMessageToNeighbors(msg', node, f)    
    
    /// <summary>
    /// Asynchronously sends a message `state` to neighbors of node "v" via f, which takes a message, the current node and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.
    /// </summary>
    /// <param name="state">The initial state or message to be passed to the neighbors of node "v". This state can be updated with each recursive call based on the return value of function "f".</param>
    /// <param name="v">The node to send the message from</param>
    /// <param name="f">The function to apply to the message, f takes a message, the current node and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.</param>
    member g.SendMessageToNeighborsAsync (state:'b, v, f) =
        async {
            let nodes =  
                Array.append (g.GetEdges v) [|for ((u,_), w) in g.InEdgesWeights v -> u, w|]
                
            for (node, weight) in nodes do
                let! msg', terminate = async { return f state (v, node, weight) }
                if terminate then () 
                else do! g.SendMessageToNeighborsAsync (msg', node, f)
        }


    member g.InEdgesWeights v =
        [| for KeyValue(a, vs) in edges do
               let w =
                   match g.GetEdgeWeight(a, v) with
                   | Some w -> w
                   | None -> failwith "no weight" //should not happen

               if vs.ContainsKey v then
                   yield ((a, v), w) |]

    member g.Ins v = 
        [|for KeyValue(a, vs) in edges do
            if vs.ContainsKey v then yield a |]

    member g.InEdges v = 
        [|for KeyValue(a, vs) in edges do
            if vs.ContainsKey v then yield (a, v) |]
 
    member g.Remove(v : 'a) =
        match edges.TryFind v with
        | None -> false
        | Some _ ->
            let removes = g.InEdges v 
            for (a,b) in removes do //all a's pointing into v=b
                edges[a].Remove b |> ignore  

            edges.Remove v 
    
    /// <summary>
    /// Adds an edge between two nodes in the graph.
    /// </summary>
    /// <param name="node1">The node where the edge starts.</param>
    /// <param name="node2">The node where the edge ends.</param>
    /// <param name="w">The weight of the edge.</param>
    /// <param name="overwriteWeight">Optional parameter. If true (or not provided), the weight of the edge will be overwritten if the edge already exists. If false, the weight of the edge will not be overwritten.</param>
    /// <returns>Returns true if the edge was successfully added or updated, false otherwise.</returns>
    member g.AddEdge(node1,node2,w,?overwriteWeight) =
        let overwrite = defaultArg overwriteWeight true 
        if not (edges.ContainsKey node1) then 
            edges.Add(node1, Dict())
         
        let connectedsToNode1 = edges[node1]
        
        if not (connectedsToNode1.ContainsKey node2) then 
            connectedsToNode1.Add(node2, w) 
        else 
            if overwrite then 
                connectedsToNode1[node2] <- w   

    member g.RemoveEdge(v1, v2, ?clearIsolatedNodes) =
        let docleanup = defaultArg clearIsolatedNodes true
        maybe {
            let! connectedToNode1 = edges.TryFind v1
            let r = connectedToNode1.Remove(v2)

            let _ =
                if docleanup && connectedToNode1.Count = 0 && Array.isEmpty (g.InEdges v1) then
                    edges.Remove v1 |> ignore

            return r
        } 

    member g.ClearIsolatedNodes() = 
        for KeyValue(v, _) in edges do
            let haschildren = 
                match edges.TryFind v with
                | Some vs -> vs.Count > 0
                | _ -> false

            if Array.isEmpty (g.Ins v) && not haschildren then 
                g.Remove v |> ignore

    member g.HasChildren v = 
        match edges.TryFind v with
        | Some vs -> vs.Count > 0
        | _ -> false

    member g.ContainsNode v = edges.ContainsKey v

    member g.AdjustWeight (v1,v2, f) = 
        match edges.TryFind v1 with
        | Some outgoingEdges -> 
            match outgoingEdges.TryFind v2 with 
            | Some w ->
                outgoingEdges[v2] <- f w 
                Some true
            | None -> Some false
        | None -> Some (false) 

    member __.EdgeWeights =
        [| for KeyValue(v1, vs) in edges do
            for KeyValue(v2, w) in vs do
                v1, v2, w |]

    member g.GetEdgeWeight (v1, v2) =  
        maybe { 
            let! vs = edges.TryFind v1
            let! w = vs.TryFind v2
            return w
        } 

    member g.ContainsEdge(v1, v2) =
        maybe {
            let! outgoingEdges = edges.TryFind v1
            return (outgoingEdges.ContainsKey v2)
        }
  
    member g.GetEdges v =
        [| match edges.TryFind v with
            | Some vs -> yield! keyValueSeqtoPairArray vs
            | _ -> () |]    
    
    member g.NodeNeighborCounts() = 
        [|for KeyValue(v1,vs) in g.EdgeData -> v1, vs.Count|]  
         
     member g.NodeNeighborCount v = 
        Option.map (fun (vs:Dict<_,_>) -> vs.Count) (edges.TryFind v) 

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

    member g.Nodes = [|for k in edges.Keys -> k|] 

    member g.ExtractShortestPath(f, (dists: Dict<_,_>, prevs: Dict<_,_>), target) =
        recurse (fst >> Option.isNone) 
            (fun (prev,l) ->
                match prev with 
                | Some p -> prevs.GetOrDefault(p, None), f(p,dists.[p]) :: l 
                | _ -> failwith "no path") 
            (Some target,[])
            |> snd

    member g.ShortestPaths source = 
        let paths = g.DijkstrasShortestPath source
        g.Nodes 
        |> Array.map (fun v -> g.ExtractShortestPath (id, paths, v))

    member g.ShortestPath(source, target) = g.ExtractShortestPath (fst, g.DijkstrasShortestPath(source, target),target)

    member g.AddEdges (edges : _ ) =
        for (v1, v2, w) in edges do 
            g.AddEdge(v1, v2, w) |> ignore 

    static member fromEdges (edges:_) =
        let g = new WeightedDirectedGraph<'a>()
        g.AddEdges edges
        g
        
    member g.DijkstrasShortestPath(source, ?target) =
        let dists = Dict.ofSeq [ source, 0. ]
        let prev = Dict()
        let vs = g.Nodes
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
                let nextEdges = g.GetEdges next
                let _ = visited.Add next 
                for (node2,weight) in nextEdges do
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
        member g.Nodes = g.Nodes
        member g.Edges = Array.map fst g.Edges
        member g.WeightedEdges = g.Edges 
        member g.Ins v = g.Ins v
        member g.GetNeighbors n = g.GetEdges n |> Array.map fst
        member g.GetWeightedEdges n = g.GetEdges n
        member g.AddNode v = g.AddNode v |> ignore
        member g.RemoveNode v = g.Remove v |> ignore
        member g.AddWeightedEdge ((v1, v2, w)) = g.AddEdge(v1, v2, w) |> ignore
        member g.AddWeightedEdges edges = g.AddEdges edges
        member g.AddEdges _ = failwith "Need weights"
        member g.GetNodeNeighborCounts() = g.NodeNeighborCounts()
        member g.GetEdgeValue (a,b) = g.GetEdgeWeight (a,b)
        member g.RawEdgeData() = Dict.ofSeq [|for KeyValue(k,v) in g.EdgeData -> k, Hashset(v.Keys)|]  
        member g.RawEdgeWeightData() = Choice1Of2 g.EdgeData   
        member g.GetNodeNeighborCount v = g.NodeNeighborCount v
        member g.ApplyToEdges f = g.ForEachEdge f 
        member g.IsDirected = true 
        member g.IsWeightNormalized = weightnormalized
        member g.HasCycles = cyclic 
        member g.AddEdge(_,_)= failwith "Need weight" 
        member g.ContainsEdge(u,v) = g.ContainsEdge(u,v)
        member g.RemoveEdge(u,v) = g.RemoveEdge(u,v,false) |> ignore
        member g.RemoveEdge(u,v,clearIsolatedNodes) = 
            g.RemoveEdge(u,v, clearIsolatedNodes) |> ignore
 

///Can have any edge type. The compressed directed graph uses an index in place of the node string to save memory. You pass in a
///'keytype to represent this index type. The reason for this is to support scenarios where you know the size limit and can 
///thus use a byte or uint16 instead of a uint32 type. An example use case I have in mind is generating hundreds of markov chains with only a 20 word vocabulary. 
///Note that a byte key will quickly run out of addresses and start overwriting nodes.
///
///if MaintainInEdges = true, a look up table for inedges is built, using a bit more memory for O(1) look up. Without, inedges lookup is O(n).
type CompressedDirectedGraph<'a, 'b, 'keytype when 'a : equality and 'a : comparison and 'a :> IEquatable<'a> and 'keytype : equality and 'keytype :> IEquatable<'keytype>>(keytype, ?MaintainInEdges) =
    let mutable edges = DictionarySlim<'keytype, DictionarySlim<'keytype, 'b>>()
    let mutable inedges = DictionarySlim<'keytype, Hashset<'keytype>>()

    let vertices = DictionarySlim<'a, 'keytype>()
    let rev_vertices = DictionarySlim<'keytype, 'a>()
    let mutable weightnormalized = false
    let mutable cyclic = None
    
    let maintainInEdges = defaultArg MaintainInEdges false

    member g.CheckForCycles() =
        match GraphAlgorithms.isCyclic g with
        | NotCyclic -> cyclic <- Some false
        | IsCyclic _ -> cyclic <- Some true 

    member g.WeightNormalized
        with get () = weightnormalized
        and set wn = weightnormalized <- wn
     
    member g.IsCyclic   
        with get() = cyclic  

    member g.EdgeData = edges

    member g.InEdgesCached = maintainInEdges

    member __.GraphData(weightsfn) =
        let edgeData =
            [| for (node1, nodes) in keyValueSeqtoPairArray edges ->
                node1, [| for (node2, w) in keyValueSeqtoPairArray nodes -> node2, weightsfn w |] |]

        let nodesData = keyValueSeqtoPairArray vertices

        let inEdgeData =
            [| for (node1, nodes) in keyValueSeqtoPairArray inedges -> node1, nodes |]

        (edgeData, nodesData), inEdgeData

    member g.RebuildInEdges () =
        let getInEdges v =
            let vset = Hashset()
            for KeyValue(a, vs) in edges do
                if vs.ContainsKey v then vset.Add a |> ignore
            vset
        
        inedges.Clear()

        for KeyValue(v,_) in rev_vertices do 
            let ins = &inedges.GetOrAddValueRef v
            ins <- getInEdges v

    member g.FromState(reprocessWeights, ((es, vs), inEdges)) =
        edges.Clear()
        vertices.Clear() 
        inedges.Clear()  
             
        let graph = DictionarySlim()
        for (node1, nodes) in es do
            let neighbors = DictionarySlim<'keytype,_>()
            for (node2,w) in nodes do            
                let weight = &neighbors.GetOrAddValueRef(node2)
                weight <- reprocessWeights w
            let node1Neighbors = &graph.GetOrAddValueRef(node1)
            node1Neighbors <- neighbors

        edges <- graph

        Array.iter (fun (item, i) ->
            let index = &vertices.GetOrAddValueRef item in index <- i) vs

        g.ComputeReverseIndex() 
        
        if maintainInEdges then 
            match Array.isEmpty inEdges with
            | true -> g.RebuildInEdges()
            | false ->
                for (i,vs) in inEdges do
                    let inset = Hashset<'keytype>()
                    for k in vs do 
                        inset.Add k |> ignore 
                    let ins = &inedges.GetOrAddValueRef i
                    ins <- inset

    member __.Clear() =
        edges.Clear()
        vertices.Clear()
        rev_vertices.Clear()
        inedges.Clear() 

    member g.ComputeReverseIndex() =
        rev_vertices.Clear()
        for (KeyValue(key, index)) in vertices do
            let item = &rev_vertices.GetOrAddValueRef index
            item <- key

    member g.AddNode(s : 'a) =
        let contained = vertices.ContainsKey s
        if not contained then
            let count = keytype vertices.Count
            let index = &vertices.GetOrAddValueRef s
            index <- count 
            let graph = &edges.GetOrAddValueRef(count)
            graph <- DictionarySlim()

            if maintainInEdges then
                let ins = &inedges.GetOrAddValueRef count
                ins <- Hashset()
        contained 

    member g.Remove(v : 'a) =
        let has, i = vertices.TryGetValue v
        if has then 
            if maintainInEdges then
                let _, ins = inedges.TryGetValue i
                for k in ins do
                    let _, es = edges.TryGetValue k
                    es.Remove i |> ignore
            else
                for (KeyValue(_, es)) in edges do
                    if es.ContainsKey i then  
                        es.Remove i |> ignore 
            edges.Remove i
        else false 
    
    member g.ClearIsolatedNodes() = 
        for KeyValue(v, i) in vertices do
            let _, es = edges.TryGetValue i
            if not (g.HasParent v) && es.Count = 0 then 
                g.Remove v |> ignore 

    member g.AddEdge(v0, v1, w) =  
        if not (vertices.ContainsKey v0) then 
            g.AddNode(v0) |> ignore
         
        if not (vertices.ContainsKey v1) then 
            g.AddNode v1 |> ignore

        let _, i = vertices.TryGetValue v0
        let _, i2 = vertices.TryGetValue v1

        let _, es = edges.TryGetValue i
        let w0 = &es.GetOrAddValueRef i2 
        w0 <- w 
        
        if maintainInEdges then
            let _, ins = inedges.TryGetValue i2
            ins.Add i |> ignore
        true 

    member g.AddEdges (edges: _ seq) = 
        for edge in edges do g.AddEdge edge |> ignore
         
    member g.RemoveEdge(v0, v1, ?clearIsolatedNodes) =
        let has1, i = vertices.TryGetValue v0
        if has1 then
            let has2, i2 = vertices.TryGetValue v1
            if has2 then
                let has3, es = edges.TryGetValue i
                if has3 then
                    if es.ContainsKey i2 then
                        es.Remove i2 |> ignore  
                if maintainInEdges then
                    let _, ins = inedges.TryGetValue i2
                    ins.Remove i |> ignore  
                let docleanup = defaultArg clearIsolatedNodes true
                let _ =
                    if docleanup && es.Count = 0
                        && Array.isEmpty (g.Ins v1) then edges.Remove i |> ignore 
                true
            else false
        else false

    member g.ContainsNode v = vertices.ContainsKey v

    member g.AdjustWeight(v1, v2, f) =
        let has1, i = vertices.TryGetValue v1
        if has1 then
            let has2, i2 = vertices.TryGetValue v2
            if has2 then
                let _, es = edges.TryGetValue i 
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
                Some(e.ContainsKey i2)
            else None
        else None

    member g.GetEdges v =
        let hasv, i = vertices.TryGetValue v
        if hasv then
            let _, es = edges.TryGetValue i
             
            [| for (KeyValue(k, w)) in es ->
                rev_vertices.TryGetValue k |> snd, w |]
        else Array.empty
    
    member g.Ins v =
        let hasv, i = vertices.TryGetValue v
        if hasv then
            if maintainInEdges then
                let _, es = inedges.TryGetValue i
                [|for n in es -> snd(rev_vertices.TryGetValue n)|]
            else 
                [| for (KeyValue(a, es)) in edges do
                    if es.ContainsKey i then
                        yield (snd (rev_vertices.TryGetValue a)) |] 
        else Array.empty 

    member g.HasParent v = 
        let hasv, i = vertices.TryGetValue v
        if hasv then
            if maintainInEdges then
                let _, es = inedges.TryGetValue i
                es.Count > 0
            else 
                edges 
                |> Seq.exists (fun (KeyValue(_, es)) -> es.ContainsKey i)

        else false

    member g.ForEachEdge(f) = 
        for KeyValue(_,nodes) in edges do
            for KeyValue(node2,_) in nodes do
                let w = &nodes.GetOrAddValueRef node2
                w <- f w

    member g.Edges =
        [|for KeyValue(node1,nodes) in g.EdgeData do
            for KeyValue(node2, w) in nodes ->
                (rev_vertices.TryGetValue node1 |> snd,
                 rev_vertices.TryGetValue node2 |> snd), w|] 

    member g.NodeNeighborCounts() = 
        [|for KeyValue(node1,nodes) in g.EdgeData ->
            rev_vertices.TryGetValue node1 |> snd, nodes.Count|]

    member g.NodeNeighborCount v = 
        let hv, i = vertices.TryGetValue v
        if hv then
            let h, d = edges.TryGetValue i
            if h then Some d.Count else None 
        else None

    member private g.RevLookUp v = rev_vertices.TryGetValue v |> snd

    member g.Nodes =
        [| for kv in vertices -> kv.Key |]

    interface IWeightedGraph<'a,'b> with 
        member g.IsDirected = true 
        member g.HasCycles = cyclic
        member g.Nodes = g.Nodes
        member g.Edges = Array.map fst g.Edges
        member g.WeightedEdges = g.Edges
        member g.GetNeighbors n = g.GetEdges n |> Array.map fst
        member g.GetWeightedEdges n = g.GetEdges n
        member g.AddNode v = g.AddNode v |> ignore 
        member g.AddWeightedEdges edges = g.AddEdges edges
        member g.AddEdges _ = failwith "Need weights"
        member g.RemoveNode v = g.Remove v |> ignore
        member g.AddWeightedEdge e = g.AddEdge e |> ignore
        member g.GetNodeNeighborCounts() = g.NodeNeighborCounts()
        member g.Ins v = g.Ins v
        member g.GetNodeNeighborCount v = g.NodeNeighborCount v
        member g.IsWeightNormalized = weightnormalized 
        member g.GetEdgeValue (a,b) = g.GetEdgeWeight (a,b)
        member g.ApplyToEdges f = g.ForEachEdge f 
        member g.AddEdge(_,_) = failwith "Need weight" 
        member g.ContainsEdge(u,v) = g.ContainsEdge(u,v)
        member g.RemoveEdge(u,v) = g.RemoveEdge(u,v,false) |> ignore
        member g.RemoveEdge(u,v,clearIsolatedNodes) = 
            g.RemoveEdge(u,v,clearIsolatedNodes) |> ignore

        member g.RawEdgeData() =
            Dict.ofSeq
                [| for KeyValue(k, v) in g.EdgeData ->
                    g.RevLookUp k,
                    Hashset [| for KeyValue(k, _) in v -> g.RevLookUp k |] |]

        member g.RawEdgeWeightData() =
            Dict.ofSeq
                [| for KeyValue(k, v) in g.EdgeData ->
                    g.RevLookUp k,
                    Dict.ofSeq [| for KeyValue(k, w) in v -> g.RevLookUp k, w |] |]
            |> Choice1Of2


type GeneralDirectedGraph<'a, 'w when 'a : equality and 'a : comparison>() =
    let mutable edges = Dict<'a, Dict<'a, 'w>>()
    let mutable weightnormalized = false
    let mutable cyclic = None

    member g.CheckForCycles() =
        match GraphAlgorithms.isCyclic g with
        | NotCyclic -> cyclic <- Some false
        | IsCyclic _ -> cyclic <- Some true 

    member g.WeightNormalized 
        with get() = weightnormalized 
        and set wn = weightnormalized <- wn

    member g.IsCyclic = cyclic  

    member g.EdgeData = edges

    member g.FromDictionary es =
        edges.Clear()
        edges <- es

    member __.Clear() = edges.Clear()

    member g.ClearIsolatedNodes() = 
        for KeyValue(v, _) in edges do
            let haschildren = 
                match edges.TryFind v with
                | Some vs -> vs.Count > 0
                | _ -> false

            if Array.isEmpty (g.Ins v) && not haschildren then 
                g.Remove v |> ignore

    member g.HasChildren v =
        match edges.TryFind v with
        | Some vs -> vs.Count > 0
        | _ -> false

    member g.ForEachEdge f =
        for (v, vs) in keyValueSeqtoPairArray edges do
            for (v2, w) in keyValueSeqtoPairArray vs do
                edges.[v].[v2] <- f w 

    member g.RemoveVerticesWhere f =
        let keys = g.Nodes
        for k in keys do
            if f k then
                g.Remove k |> ignore
                 
    member g.RemoveEdgesWhere f =
        for (v, vs) in keyValueSeqtoPairArray edges do
            for (v2, w) in keyValueSeqtoPairArray vs do
                if f ((v,v2),w) then g.RemoveEdge(v,v2) |> ignore

    member g.RemoveEdge(v1,v2, ?clearIsolatedNodes) =
        let docleanup = defaultArg clearIsolatedNodes true
        maybe {
            let! connectedToNode1 = edges.TryFind v1
            let r = connectedToNode1.Remove(v2)

            let _ =
                if docleanup && connectedToNode1.Count = 0
                    && Array.isEmpty (g.InEdges v1) then edges.Remove v1 |> ignore 
            return r
        } 

    member g.AddNode(s : 'a) =
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s, Dict())
        contained
 
    member g.AddEdge(v0, v1, w, ?overwrite) = 
        let shouldOverwrite = defaultArg overwrite true

        if not (edges.ContainsKey v0) then
            edges.Add(v0, Dict())

        edges[v0].ExpandElseAdd(v1, (fun w0 -> if shouldOverwrite then w else w0), w)
 

    member g.AddEdges(edges) = 
        for (v1, v2, w)  in edges do g.AddEdge(v1, v2, w) 

    static member fromEdges (edges) =
        let g = new GeneralDirectedGraph<'a,'b>()
        g.AddEdges edges
        g

    /// <summary>
    /// Sends a message `msg` to child nodes of node "v" via f, which takes a message, the current node, edge weight and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.
    /// </summary>
    /// <param name="msg">The message to send</param>
    /// <param name="v">The node to send the message from</param>
    /// <param name="f">The function to apply to the message, f takes a message, the current node, edge weight and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.</param>
    member g.SendMessageToChildren(msg:'b, v, f) =
        let neighbors = g.GetEdges v 
        for (node, weight) in neighbors do
            let msg', terminate = f msg (v, node, weight)
            if terminate then () else g.SendMessageToChildren(msg', node, f)

    /// <summary>
    /// Asynchronously sends a message `msg` to child nodes of node "v" via f, which takes a message, the current node, edge weight and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.
    /// </summary>
    /// <param name="msg">The message to send</param>
    /// <param name="v">The node to send the message from</param>
    /// <param name="f">The function to apply to the message, f takes a message, the current node, edge weight and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.</param>
    member g.SendMessageToChildrenAsync(msg:'b, v, f) = 
        async {
            let neighbors = g.GetEdges v 
            for (node, weight) in neighbors do
                let! msg', terminate = async {return f msg (v, node, weight)}
                if terminate then () 
                else do! g.SendMessageToChildrenAsync(msg', node, f)
        }  

    /// <summary>
    /// Sends a message `msg` to parent nodes of node "v" via f, which takes a message, the current node, edge weight and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.
    /// </summary>
    /// <param name="msg">The message to send</param>
    /// <param name="v">The node to send the message from</param>
    /// <param name="f">The function to apply to the message, f takes a message, the current node, edge weight and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.</param>
    member g.SendMessageToParents(msg:'b, v, f) =
        let nodes = g.InEdgesWeights v 
        for ((node, _), weight) in nodes do
            let msg', terminate = f msg (v, node, weight)
            if terminate then () else g.SendMessageToParents(msg', node, f)

    /// <summary>
    /// Asynchronously sends a message `msg` to parent nodes of node "v" via f, which takes a message, the current node, edge weight and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.
    /// </summary>
    /// <param name="msg">The message to send</param>
    /// <param name="v">The node to send the message from</param>
    /// <param name="f">The function to apply to the message, f takes a message, the current node, edge weight and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.</param>
    member g.SendMessageToParentsAsync(msg:'b, v, f) =
         async {
            let nodes = g.InEdgesWeights v 
            for ((node, _), weight) in nodes do
                let! msg', terminate = async { return f msg (v, node, weight)}
                if terminate then () 
                else do! g.SendMessageToParentsAsync(msg', node, f)
        }  
    /// <summary>
    /// Sends a message `msg` to neighbor nodes of node "v" via f, which takes a message, the current node, edge weight and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.
    /// </summary>
    /// <param name="msg">The message to send</param>
    /// <param name="v">The node to send the message from</param>
    /// <param name="f">The function to apply to the message, f takes a message, the current node, edge weight and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.</param>
    member g.SendMessageToNeighbors (msg:'b, v, f) =
        let nodes = g.GetEdges v
        let allnodes = Array.append nodes [|for ((u,_), w) in g.InEdgesWeights v -> u, w|]
            
        for (node, weight) in allnodes do
            let msg', terminate = f msg v node weight
            if terminate then () else g.SendMessageToNeighbors(msg', node, f)

    /// <summary>
    /// Asynchronously sends a message `msg` to neighbor nodes of node "v" via f, which takes a message, the current node, edge weight and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.
    /// </summary>
    /// <param name="msg">The message to send</param>
    /// <param name="v">The node to send the message from</param>
    /// <param name="f">The function to apply to the message, f takes a message, the current node, edge weight and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.</param>
    member g.SendMessageToNeighborsAsync (msg:'b, v, f) =
        async {
            let nodes = Array.append (g.GetEdges v) [|for ((u,_), w) in g.InEdgesWeights v -> u, w|]
            for (node, weight) in nodes do
                let! msg', terminate = async { return f msg (v, node, weight) }
                if terminate then () 
                else do! g.SendMessageToNeighborsAsync(msg', node, f)
        } 
    
    member g.InEdgesWeights v =
        [|for KeyValue(a, vs) in edges do
            let w = match g.GetEdgeWeight(a, v) with Some w -> w | None -> failwith "no weight"
            if vs.ContainsKey v then yield ((a, v), w) |]                

    member g.Remove(v : 'a) =
        match (edges.TryFind v) with
        | None -> false
        | Some _ ->
            for (KeyValue(_, es)) in edges do
                if es.ContainsKey v then
                    es.Remove v |> ignore 
            edges.Remove v

    member g.InEdges v =  
        [|for KeyValue(a, vs) in edges do
            if vs.ContainsKey v then yield (a, v) |]

    member g.Ins v = 
        [|for KeyValue(a, vs) in edges do
            if vs.ContainsKey v then yield a |]


    member g.ContainsNode v = edges.ContainsKey v

    member g.AdjustWeight(v1, v2, f) =
        maybe {
            let! es = edges.TryFind v1
            let! oldweight = es.TryFind v2
            es.[v2] <- f oldweight
            return true
        }

    member g.GetEdgeWeight (v1, v2) = maybe { 
        let! es = edges.TryFind v1
        let! w = es.TryFind v2
        return w }

    member g.ContainsEdge (v1, v2) = maybe { 
        let! elist0 = edges.TryFind v1
        return (elist0.ContainsKey v2) }

    member g.GetEdgesRaw v =
        seq {
            match edges.TryFind v with
            | Some edgelist -> yield! edgelist
            | _ -> ()
        }

    member g.GetEdges v = 
        keyValueSeqtoPairArray (g.GetEdgesRaw v)

    member g.Edges = 
        [|for KeyValue(v1,vs) in g.EdgeData do
            for KeyValue(v2,w) in vs -> (v1,v2), w |]  

    member g.Nodes = [|for k in edges.Keys -> k|]  

    member g.NodeNeighborCounts() = 
        [|for KeyValue(v1,vs) in g.EdgeData -> v1, vs.Count|]  

    member g.NodeNeighborCount v = 
        Option.map (fun (vs:Dict<_,_>) -> vs.Count) (edges.TryFind v)  

    interface IWeightedGraph<'a,'w> with
        member g.Nodes = g.Nodes
        member g.Edges = Array.map fst g.Edges
        member g.WeightedEdges = g.Edges
        member g.GetNeighbors n = g.GetEdges n |> Array.map fst
        member g.GetWeightedEdges n = g.GetEdges n 
        member g.IsDirected = true
        member g.AddNode v = g.AddNode v |> ignore
        member g.RemoveNode v = g.Remove v |> ignore
        member g.Ins v = g.Ins v
        member g.AddEdges _ = failwith "Need weights"
        member g.AddWeightedEdge((v1, v2, w))  = g.AddEdge (v1, v2, w)  |> ignore
        member g.AddWeightedEdges edges = g.AddEdges edges
        member g.GetNodeNeighborCounts() = g.NodeNeighborCounts() 
        member g.ApplyToEdges f = g.ForEachEdge f 
        member g.AddEdge(_,_)= failwith "Need weight"
        member g.GetEdgeValue (a,b) = g.GetEdgeWeight (a,b)
        member g.RawEdgeData() =
            Dict.ofSeq
                [| for KeyValue(k, v) in g.EdgeData -> k, Hashset(v.Keys) |]

        member g.RawEdgeWeightData() = Choice1Of2 g.EdgeData  
        member g.HasCycles = cyclic
        member g.GetNodeNeighborCount v = g.NodeNeighborCount v
        member g.IsWeightNormalized = weightnormalized 
        member g.ContainsEdge(u,v) = g.ContainsEdge(u,v)
        member g.RemoveEdge(u,v) = g.RemoveEdge(u,v,false) |> ignore
        member g.RemoveEdge(u,v,clearIsolatedNodes) = g.RemoveEdge(u,v,clearIsolatedNodes) |> ignore
         

type DirectedMultiGraph<'a, 'w when 'a: equality>() =
   let mutable edges = Dict<'a, Dict<'a, 'w ResizeArray>>()
    
   let numberOfChildren n =
       match edges.TryFind n with
       | Some a -> a.Count
       | None -> 0
   let mutable weightnormalized = false
   let mutable cyclic = None

   member g.WeightNormalized 
       with get() = weightnormalized 
       and set wn = weightnormalized <- wn

   member g.IsCyclic = cyclic  

   member g.Nodes = g.EdgeData |> Seq.map keyValueToKey |> Seq.toArray

   member g.Edges =
       [| for (KeyValue (e, e2s)) in edges do
             for (KeyValue (e2, ws)) in e2s do
                 for w in ws do
                     yield ((e, e2), w) |]

    member g.CheckForCycles() =
        match GraphAlgorithms.isCyclic g with
        | NotCyclic -> cyclic <- Some false
        | IsCyclic _ -> cyclic <- Some true 

   member g.FromDictionary(edgesDict) =
       edges.Clear()
       edges <- edgesDict 

   member g.AddNode(s: 'a) =
       let contained = edges.ContainsKey s

       if not contained then
           edges.Add(s, Dict())

       contained

   member g.Remove(v) =
       match (edges.TryFind v) with
       | None -> false
       | Some elist ->
           for KeyValue (_, es) in edges do
               if es.ContainsKey v then
                   es.[v].Clear()
                   es.Remove v |> ignore

           elist.Clear()
           edges.Remove v

   member g.EdgeData = edges

   member g.AddEdge(v0, v1, w) =  
        if not (edges.ContainsKey v0) then
            edges.Add(v0, Dict())

        let outgoingEdges = edges[v0]
        match outgoingEdges.TryFind v1 with
        | Some ws -> ws.Add w
        | None -> outgoingEdges.Add(v1, ResizeArray [w])    
       
    member g.AddEdges (edges: _ seq) = 
        for (v1, v2, w) in edges do g.AddEdge(v1, v2, w)

    member g.RemoveEdge(v1,v2, ?clearIsolatedNodes) =
       let docleanup = defaultArg clearIsolatedNodes true
       maybe {
           let! connectedToNode1 = edges.TryFind v1
           let r = connectedToNode1.Remove(v2)

           let _ =
               if docleanup && connectedToNode1.Count = 0
                   && Array.isEmpty (g.InEdges v1) then edges.Remove v1 |> ignore 
           return r
       } 

   static member fromEdges (edges:_) =
        let g = new DirectedMultiGraph<'a,'b>()
        g.AddEdges edges
        g 

   ///f informs whether the node is penultimate or not.
   member g.ModifyNodeEdges f n =
       match edges.TryFind n with
       | Some nodes ->
           let keys = Seq.toArray nodes.Keys
           let ispenultimate = keys |> Array.sumBy numberOfChildren = 0

           for k in keys do
               let currnodes = nodes.[k].ToArray()
               nodes.[k].Clear()
               nodes.[k] <- f ispenultimate currnodes
       | None -> ()

   member g.ContainsNode v = edges.ContainsKey v

   member g.ContainsEdge (v1, v2) =
       maybe {
           let! edges = edges.TryFind v1
           return (edges.ContainsKey v2)
       }
  
   member g.GetEdges v =
       match edges.TryFind v with
       | None -> [||]
       | Some edgeList ->
           [| for KeyValue (k, ws) in edgeList do
                  for w in ws -> k, w |] 

   member g.ForEachEdge f =
       for KeyValue(v, vs) in edges do
           for KeyValue(v2, ws) in vs do 
               for i in 0..ws.Count - 1 do ws[i] <- f ws[i] 
   
    member g.GetEdgeWeight (v1, v2) = 
        maybe { 
            let! es = edges.TryFind v1
            let! w = es.TryFind v2
            return w |> Seq.toArray 
        }

   member g.Ins v = 
       [|for KeyValue(a, vs) in edges do
           if vs.ContainsKey v then yield a |]

   member g.InEdges v =  
       [|for KeyValue(a, vs) in edges do
           if vs.ContainsKey v then yield (a, v) |]

   member g.NodeNeighborCounts() = 
       [|for KeyValue(v1,vs) in g.EdgeData -> v1, vs.Count|]  

   member g.NodeNeighborCount v = 
       Option.map (fun (vs:Dict<_,_>) -> vs.Count) (edges.TryFind v) 

   interface IWeightedGraph<'a,'w> with
       member g.Nodes = g.Nodes
       member g.Edges = Array.map fst g.Edges
       member g.WeightedEdges = g.Edges 
       member g.Ins v = g.Ins v
       member g.GetNeighbors n = g.GetEdges n |> Array.map fst
       member g.GetWeightedEdges n = g.GetEdges n
       member g.AddNode v = g.AddNode v |> ignore
       member g.RemoveNode v = g.Remove v |> ignore
       member g.AddWeightedEdge e = g.AddEdge e
       member g.AddWeightedEdges edges = g.AddEdges edges
       member g.GetNodeNeighborCounts() = g.NodeNeighborCounts()
       member g.GetEdgeValue (a,b) = failwith "Not compatible"
       member g.RawEdgeData() = Dict.ofSeq [|for KeyValue(k,v) in g.EdgeData -> k, Hashset(v.Keys)|]  
       member g.RawEdgeWeightData() = failwith "Not compatible" 
       member g.GetNodeNeighborCount v = g.NodeNeighborCount v
       member g.ApplyToEdges f = g.ForEachEdge f 
       member g.IsDirected = true 
       member g.IsWeightNormalized = weightnormalized
       member g.HasCycles = cyclic 
       member g.AddEdge(_,_)= failwith "Need weights" 
       member g.AddEdges _ = failwith "Need weights"
       member g.ContainsEdge(u,v) = g.ContainsEdge(u,v)
       member g.RemoveEdge(u,v) = g.RemoveEdge(u,v,false) |> ignore
       member g.RemoveEdge(u,v,clearIsolatedNodes) = 
           g.RemoveEdge(u,v, clearIsolatedNodes) |> ignore


type DirectedGraph<'a when 'a : equality and 'a : comparison>() =
    let mutable edges = Dict<'a,Hashset<'a>>() 
     
    let mutable cyclic = None 
     
    member g.IsCyclic = cyclic  

    member __.Clear() = edges.Clear() 

    member g.CheckForCycles() =
        match GraphAlgorithms.isCyclic g with
        | NotCyclic -> cyclic <- Some false
        | IsCyclic _ -> cyclic <- Some true 

    member g.EdgeData = edges 

    member g.ToWeightedGraph() =
        let wg = WeightedDirectedGraph()
        [|for KeyValue(u,vs) in edges ->
            u, Dict.ofSeq [|for v in vs -> v, 1.|] |]
        |> Dict.ofSeq
        |> wg.FromDictionary  
        wg

    member g.RemoveVerticesWhere f =
        let keys = g.Nodes
        for k in keys do
            if f k then
                g.Remove k |> ignore
                 
    member g.RemoveEdgesWhere f =
        for (v, vs) in keyValueSeqtoPairArray edges do
            for v2 in vs do
                if f ((v,v2)) then g.RemoveEdge(v,v2) |> ignore

    member g.FromDictionary(edgesDict) =
        edges.Clear()
        edges <- edgesDict 

    member g.AddNode(s : 'a) =
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s, Hashset())
        contained 

    member g.InEdges v = 
        [|for KeyValue(a, vs) in edges do
            if vs.Contains v then yield (a, v) |]

    member g.Ins v = 
        [|for KeyValue(a, vs) in edges do
            if vs.Contains v then yield a |]
             
    member g.Remove(v : 'a) =
        match (edges.TryFind v) with
        | None -> false
        | Some _ ->  
            let removes = g.InEdges v 
            for (a,b) in removes do //all a's pointing into v=b
                edges.[a].Remove b |> ignore 
            edges.Remove v  

    member g.AddEdge(node1,node2) =  
        if not (edges.ContainsKey node1) then
            edges.Add(node1, Hashset())
        edges[node1].Add node2

    member g.AddEdges (edges: _ seq) = 
        for (v1, v2) in edges do g.AddEdge(v1, v2) |> ignore

    member g.RemoveEdge(v1,v2, ?clearIsolatedNodes) =
        let docleanup = defaultArg clearIsolatedNodes true
        maybe {
            let! connectedToNode1 = edges.TryFind v1
            let r = connectedToNode1.Remove(v2)

            let _ =
                if docleanup && connectedToNode1.Count = 0
                    && Array.isEmpty (g.InEdges v1) then edges.Remove v1 |> ignore 
            return r
        } 

    member g.ContainsNode v = edges.ContainsKey v 

    member g.ContainsEdge (v1, v2) = maybe { 
        let! elist0 = edges.TryFind v1
        return (elist0.Contains v2) }

    member g.GetEdgesRaw v = 
        match edges.TryFind v with
        | Some edgelist -> Hashset(edgelist)
        | _ -> Hashset() 

    member g.GetEdges v = Seq.toArray (g.GetEdgesRaw v)

    member g.Edges = 
        [|for KeyValue(v1,vs) in g.EdgeData do
            for v2 in vs -> (v1,v2)|]   

    member g.Nodes = [|for k in edges.Keys -> k|]  

    member g.NodeNeighborCounts() =
        [|for KeyValue(v1,vs) in g.EdgeData -> (v1,vs.Count)|]   

    member g.NodeNeighborCount v = 
        Option.map (fun (vs:Hashset<_>) -> vs.Count) (edges.TryFind v)  
    
    static member ofChain l =   
        let dg = DirectedGraph() 

        List.pairwise l 
        |> List.iter (dg.AddEdge >> ignore)

        dg
 
    static member fromEdges (edges:_) =
        let g = new DirectedGraph<'a>()
        g.AddEdges edges
        g

    interface IGraph<'a> with
        member g.Nodes = g.Nodes
        member g.Edges = g.Edges 
        member g.GetNeighbors n = g.GetEdges n  
        member g.IsDirected = true
        member g.Ins v = g.Ins v
        member g.AddNode v = g.AddNode v |> ignore
        member g.RemoveNode v = g.Remove v |> ignore 
        member g.GetNodeNeighborCounts() = g.NodeNeighborCounts()
        member g.RawEdgeData() = g.EdgeData  
        member g.HasCycles = cyclic
        member g.GetNodeNeighborCount v = g.NodeNeighborCount v   
        member g.ContainsEdge(u,v) = g.ContainsEdge(u,v)
        member g.RemoveEdge(u,v) = g.RemoveEdge(u,v,false) |> ignore
        member g.AddEdge(u,v) = g.AddEdge(u,v) |> ignore
        member g.AddEdges edges = g.AddEdges edges
        member g.RemoveEdge(u,v,clearIsolatedNodes) = 
            g.RemoveEdge(u,v, clearIsolatedNodes) |> ignore