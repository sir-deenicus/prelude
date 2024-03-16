module Prelude.SimpleGraphs

open Prelude.Common
open System
open Prelude.Collections.FibonacciHeap

type IGraph<'Node> = 
    abstract IsDirected : bool   
    abstract HasCycles : bool option 
    abstract Vertices : 'Node []
    abstract Edges : ('Node * 'Node) [] 
    abstract RawEdgeData : unit -> Dict<'Node, Hashset<'Node>>
    abstract AddNode : 'Node -> unit
    abstract RemoveNode : 'Node -> unit
    abstract GetNeighbors : 'Node -> 'Node [] option
    abstract GetNodeNeighborCounts : unit -> ('Node * int) [] 
    abstract GetNodeNeighborCount : 'Node -> int option
    abstract Ins : 'Node -> 'Node []  
    abstract RemoveEdge : 'Node * 'Node * bool -> unit
    abstract RemoveEdge : 'Node * 'Node -> unit
    abstract InsertEdge : 'Node * 'Node -> unit
    abstract ContainsEdge : 'Node * 'Node -> bool option 

type IWeightedGraph<'Node, 'Weight> =
    inherit IGraph<'Node> 
    abstract WeightedEdges : (('Node * 'Node) * 'Weight) [] 
    abstract RawEdgeWeightData : unit -> Dict<'Node, Dict<'Node, 'Weight>>
    abstract GetWeightedEdges : 'Node -> ('Node * 'Weight) [] option 
    abstract GetEdgeValue : 'Node * 'Node -> 'Weight option 
    abstract InsertWeightedEdge : ('Node*'Node*'Weight) -> unit
    abstract GetRawEdges : 'Node -> Dict<'Node,'Weight> option
    abstract ApplyToEdges : ('Weight -> 'Weight) -> unit
    abstract IsWeightNormalized : bool

type UndirectedGraph<'a when 'a: equality and 'a: comparison>() = 
    let mutable edges = Dict<'a,'a Hashset>()
    member g.EdgeData = edges   
    
    member g.InsertNode(v) =
        let contained = edges.ContainsKey v
        if not contained then edges.Add(v,Hashset())
        contained

    member g.FromDictionary(edgesraw) =
       edges.Clear()
       edges <- edgesraw

    member g.InsertEdge(v0,v1) = 
        let elist0 =
            match edges.TryFind v0 with
            | Some elist -> elist 
            | None ->  
                edges.Add(v0,Hashset())
                edges[v0]
        let elist1 =
            match edges.TryFind v1 with
            | Some elist -> elist 
            | None ->  
                edges.Add(v1,Hashset())
                edges[v1]
        let in0 = elist0.Add v1
        let in1 = elist1.Add v0
        in0, in1
  
            
    static member fromEdges (tuples:_) =
        let g = new UndirectedGraph<'a>()
        g.AddEdges tuples
        g 

    member g.Clear() = edges.Clear()

    member g.AddEdges(edges) =
        for (v1, v2) in edges do g.InsertEdge(v1, v2) |> ignore

    member g.Remove(v: 'a) =
        match (edges.TryFind v) with
        | None -> false
        | Some elist -> 
            for v2 in elist do 
                edges.[v2].Remove(v) |> ignore
                if edges.[v2].Count = 0 then ignore(edges.Remove v2)
            edges.Remove v
    
    member g.RemoveEdge(v0,v1) =
        maybe {
            let! elist0 = edges.TryFind v0
            let! elist1 = edges.TryFind v1
            let in0 = elist0.Remove v1
            let in1 = elist1.Remove v0
            let _ = if elist0.Count = 0 then edges.Remove v0 |> ignore
            let _ = if elist1.Count = 0 then edges.Remove v1 |> ignore
            return (in0,in1)
        }

    member g.Vertices = [|for KeyValue(k,_) in g.EdgeData -> k|] 

    member g.Edges =
        [|for KeyValue(v1,vs) in g.EdgeData do
            for v2 in vs -> Pair.lessToLeft(v1,v2) |] 
        |> Hashset
        |> Seq.toArray

    /// <summary>
    /// Sends a message `state` to neighbors of node "v" via f, which takes a message, the current node and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.
    /// </summary>
    /// <param name="state">The initial state or message to be passed to the neighbors of node "v". This state can be updated with each recursive call based on the return value of function "f".</param>
    /// <param name="v">The node to send the message from</param>
    /// <param name="f">The function to apply to the message, f takes a message, the current node and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.</param>
    member g.SendMessageToNeighbors (state:'b, v, f) =
        let neighbors = g.GetEdges v
        match neighbors with
        | None -> ()
        | Some nodes -> 
            for node in nodes do
                let msg', terminate = f state (v, node)
                if terminate then () else g.SendMessageToNeighbors(msg', node, f) 
     
    member g.ContainsVertex v = edges.ContainsKey v

    member g.ContainsEdge (v1,v2) = maybe {
        let! elist0 = edges.TryFind v1
        return (elist0.Contains v2) }

    member g.GetEdges v = maybe {
        let! elist = edges.TryFind v
        return elist |> Seq.toArray }

    member g.NodeNeighborCounts () = 
        [|for KeyValue(v1,vs) in g.EdgeData -> v1, vs.Count |]  
    
    member g.NodeNeighborCount v = 
        Option.map (fun (vs:Hashset<_>) -> vs.Count) (edges.TryFind v)


    interface IGraph<'a> with
        member g.Vertices = g.Vertices
        member g.Edges = g.Edges  
        member g.GetNeighbors n = g.GetEdges n
        member g.IsDirected = false  
        member g.Ins _ = failwith "No a directed graph"
        member g.RawEdgeData() = g.EdgeData 
        member g.HasCycles = None
        member g.GetNodeNeighborCounts() = g.NodeNeighborCounts()
        member g.GetNodeNeighborCount v = g.NodeNeighborCount v
        member g.AddNode v = g.InsertNode v |> ignore 

        member g.RemoveNode v = g.Remove v |> ignore

        member g.InsertEdge(u,v) = g.InsertEdge(u,v) |> ignore
        member g.RemoveEdge(u,v) = g.RemoveEdge(u,v) |> ignore
        member g.RemoveEdge(u,v, clean) = failwith "No a directed graph"
        member g.ContainsEdge(u,v) = g.ContainsEdge(u,v) 

//===================================================//

[<Struct;CustomComparison;CustomEquality>]
type WeightPair<'a when 'a: comparison> =
    val Weight: float
    val VertX: 'a
    val VertY: 'a    
    new(w: float,x,y) =
        {Weight = w ; VertX = x; VertY = y}    
    override g.ToString() =
        (string g.Weight) + ", " + (g.VertX.ToString()) + ","  + (g.VertY.ToString())    
    interface IEquatable<WeightPair<'a>> with
        member this.Equals(other) =
            this.Weight = other.Weight && this.VertX = other.VertX 
            && this.VertY = other.VertY    
    interface IComparable<WeightPair<'a>> with
        member this.CompareTo(other) =
            if this.Weight = other.Weight then 
                compare (this.VertX,this.VertY) (other.VertX,other.VertY)
            else this.Weight.CompareTo(other.Weight)

//-----------------

[<Struct; CustomComparison; CustomEquality>]
type WeightedNode<'a when 'a: comparison> =
    val Weight: float
    val Node: 'a
    new(w: float, x) = { Weight = w; Node = x }

    override g.ToString() =
        (string g.Weight) + ", " + (g.Node.ToString())

    interface IEquatable<WeightedNode<'a>> with
        member this.Equals(other) =
            this.Weight = other.Weight && this.Node = other.Node

    interface IComparable<WeightedNode<'a>> with
        member this.CompareTo(other) =
            if this.Weight = other.Weight then
                compare this.Node other.Node
            else
                this.Weight.CompareTo(other.Weight)

//===================================================
///Fastweights is better for lots of look ups and adjusting of weights, otherwise overhead is not worth it.
type WeightedGraph<'a when 'a: equality and 'a: comparison>(?fastweights) =
    let mutable edges = Dict<'a,Dict<'a, float>>()
    let mutable edgeWeights =
        Dict<struct ('a * 'a),float>(HashIdentity.Structural)
    let dofastweights = defaultArg fastweights false
    let mutable weightnormalized = false

    member g.WeightNormalized
        with get () = weightnormalized
        and set wn = weightnormalized <- wn
      
    member g.EdgeData = edges 

    member g.ForEachEdge f =
        for (v, vs) in keyValueSeqtoPairArray edges do
            for (v2, w) in keyValueSeqtoPairArray vs do
                edges.[v].[v2] <- f w

    member g.RemoveVerticesWhere f =
        let keys = g.Vertices
        for k in keys do
            if f k then
                g.Remove k |> ignore
                 
    member g.RemoveEdgesWhere f =
        for (v, vs) in keyValueSeqtoPairArray edges do
            for (v2, w) in keyValueSeqtoPairArray vs do
                if f ((v,v2),w) then g.RemoveEdge(v,v2) |> ignore

    member g.InsertRange(edgesRaw,?edgeweights) =
        edges.Clear()
        edges <- edgesRaw
        edgeWeights.Clear()
        if edgeweights.IsSome then
           edgeWeights <- edgeweights.Value
        
    member g.InsertNode(s: 'a) =
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s,Dict())
        contained
    
    member g.Remove(v: 'a) =
        match (edges.TryFind v) with //find edges
        | None -> false
        | Some edgelist -> //nodes connected to
            for KeyValue(connectedNode,_) in edgelist do 
                if dofastweights then
                    let vertless,vertHigher = Pair.lessToLeft(v,connectedNode)
                    let node = struct (vertless,vertHigher)
                    match edgeWeights.TryFind node with
                    | Some _ -> ignore (edgeWeights.Remove node)
                    | _ -> ()
                edges.[connectedNode].Remove(v) |> ignore
                if edges.[connectedNode].Count = 0 then ignore(edges.Remove connectedNode)
            edges.Remove v
    
    member g.InsertEdge(v0,v1,w) =
        let node1,node2 = Pair.lessToLeft(v0,v1)
        if not dofastweights || not(edgeWeights.ContainsKey(struct (node1,node2))) then 
            maybe {
                let! connectedsToNode1 = edges.TryFind node1
                if connectedsToNode1.ContainsKey node2 then 
                   return false
                else
                    let! node2connecteds = edges.TryFind node2
                    connectedsToNode1.Add(node2, w)
                    node2connecteds.Add(node1, w)
                
                    let _ =
                        if dofastweights then 
                            ignore(edgeWeights.ExpandElseAdd (struct (node1,node2), id, w))
                    return true
            }
        else None 

    member g.InsertFromTuples tuples =
        for (v1,v2,w) in tuples do
            match v1 with 
            | None ->
                g.InsertNode v2 |> ignore
            | Some v1 ->
                g.InsertNode v1 |> ignore
                g.InsertNode v2 |> ignore
                g.InsertEdge(v1,v2,w) |> ignore

    static member fromTuples tuples =
        let g = new WeightedGraph<'a>()
        g.InsertFromTuples tuples
        g


    member g.RemoveEdge(v1,v2) =
        maybe {
            let! connectedToNode1 = edges.TryFind v1 
            let! node2connecteds = edges.TryFind v2
            let r = connectedToNode1.Remove(v2)
            let _ = node2connecteds.Remove(v1)
            let _ = if connectedToNode1.Count = 0 then edges.Remove v1 |> ignore
            let _ = if node2connecteds.Count = 0 then edges.Remove v2 |> ignore

            let _ =
                if dofastweights then  
                    let node1,node2 = Pair.lessToLeft(v1,v2) 
                    ignore(edgeWeights.Remove (struct (node1,node2)))
            return r
        } 

    member g.ContainsVertex v = edges.ContainsKey v
    
    member g.AdjustWeight (v1,v2, f) =
        let vertless,vertHigher = Pair.lessToLeft(v1,v2)
        if dofastweights then 
            maybe {       
                match edgeWeights.TryFind(struct (vertless,vertHigher)) with
                | Some w -> 
                    edgeWeights.[struct (vertless,vertHigher)] <- f w
                    return true
                | None -> return! g.InsertEdge(v1,v2,f 0.)   
            }
        else  
            match edges.TryFind v1 with
            | Some elistV1 ->
                match edges.TryFind v2 with 
                | Some elistV2 ->  
                    match elistV1.TryFind v2 with 
                    | Some w ->
                        elistV1.[v2] <- f w
                        elistV2.[v1] <- f w
                        Some true
                    | None -> g.InsertEdge(v1,v2, f 0.)
                | None -> Some (false)
            | None -> Some (false) 
    
    member __.EdgeWeights = edgeWeights

    member g.GetEdgeWeight (v1, v2) =  
        maybe {
            if dofastweights then
                let a,b = Pair.lessToLeft(v1,v2)
                let! w = edgeWeights.TryFind(struct (a,b))
                return w 
            else
                let! vs = edges.TryFind v1
                let! w = vs.TryFind v2
                return w
        }  

    /// <summary>
    /// Sends a message `msg` to neighbors of node "v" via f, which takes a message, the current node and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.
    /// </summary>
    /// <param name="msg">The message to send</param>
    /// <param name="v">The node to send the message from</param>
    /// <param name="f">The function to apply to the message, f takes a message, the current node, edge weight, and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.</param>
    member g.SendMessageToNeighbors (msg:'b, v, f) =
        let nodes = 
            match g.GetEdges v with
            | Some nodes -> nodes
            | None -> [||]
        for (node, weight) in nodes do
            let msg', terminate = f msg v node weight
            if terminate then () else g.SendMessageToNeighbors (msg', node, f)


    /// <summary>
    /// Asynchronously sends a message `msg` to neighbors of node "v" via f, which takes a message, the current node and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.
    /// </summary>
    /// <param name="msg">The message to send</param>
    /// <param name="v">The node to send the message from</param>
    /// <param name="f">The function to apply to the message, f takes a message, the current node, edge weight, and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.</param>
    member g.SendMessageToNeighborsAsync (msg:'b, v, f) =
        async {
            let nodes = 
                match g.GetEdges v with
                | Some nodes -> nodes
                | None -> [||]
            for (node, weight) in nodes do
                let! msg', terminate = f msg v node weight
                if terminate then () else g.SendMessageToNeighborsAsync (msg', node, f)
        } |> Async.Start 

    member g.ContainsEdge (v1, v2) = maybe {
        let! elist0 = edges.TryFind v1
        return (elist0.ContainsKey v2) }
    
    member g.GetEdgesRaw v = maybe {
        let! elist = edges.TryFind v
        return elist}
    
    member g.GetEdges v = Option.map keyValueSeqtoPairArray (g.GetEdgesRaw v)
    
    member g.Edges = 
        [|for KeyValue(v1,vs) in g.EdgeData do
            for KeyValue(v2,w) in vs -> Pair.lessToLeft (v1,v2), w |]
        |> Hashset
        |> Seq.toArray 
    
    member g.OrderedEdges =
        let sorted = Collections.Generic.SortedSet() 
        for KeyValue(v1,vs) in g.EdgeData do
            for KeyValue(v2,w) in vs do 
                sorted.Add (WeightedNode(w, Pair.lessToLeft(v1,v2)))
                |> ignore
        [|for v in sorted -> v.Node, v.Weight|]
    
    member g.NodeNeighborCounts() = 
        [|for KeyValue(v1,vs) in g.EdgeData -> v1, vs.Count |] 

    member g.NodeNeighborCount v = 
        Option.map (fun (vs:Dict<_,_>) -> vs.Count) (edges.TryFind v)

    ///if do max=true it does maximum spanning tree instead
    member g.MinimumSpanningTree(?domax) =
        let dir =
            if (defaultArg domax false) then -1.
            else 1.
        
        let currentCut = Hashset(edges.Keys)
        let root = currentCut |> Seq.head
        let tree = WeightedGraph()
        let fs = Collections.Generic.SortedSet()
        
        let _,_,steps =
            recurse (fun _ -> currentCut.Count = 0) (fun (node1,i,getnodes) -> 
                if getnodes then 
                    for KeyValue(node2, weight) in edges.[node1] do
                        if currentCut.Contains node2 
                           || currentCut.Contains node1 then 
                            ignore(fs.Add(WeightPair(weight * dir,node1,node2)))
                if fs.Count = 0 then (currentCut |> Seq.head,i + 1,true)
                else 
                    let v1,v2,w,next =
                        let minel = fs.Min
                        minel.VertX,minel.VertY,minel.Weight,minel
                    
                    let _ = fs.Remove next
                    if (currentCut.Contains v1 || currentCut.Contains v2) then 
                        let _ = tree.InsertNode v1
                        let _ = tree.InsertNode v2
                        let _ = tree.InsertEdge(v1,v2,w)
                        let _ = currentCut.Remove v1
                        let _ = currentCut.Remove v2
                        (v2,i + 1,true)
                    else (node1,i + 1,false)) (root,0,true)
        tree
    
    member g.Vertices = [|for k in edges.Keys -> k|] 

    member g.ExtractShortestPath (f, (dists: Dict<_,_>, prev: Dict<_,_>), target) =
        recurse (fst >> Option.isNone) 
            (fun (path,l) ->
                match path with 
                | Some p -> prev.GetOrDefault(p, None), f(p,dists.[p]) :: l 
                | _ -> failwith "no path") 
            (Some target,[])
            |> snd

    member g.ShortestPaths source = 
        let paths = g.DijkstrasShortestPath source
        g.Vertices 
        |> Array.map (fun v -> g.ExtractShortestPath(id, paths, v))

    member g.ShortestPath (source, target) = g.ExtractShortestPath (fst, g.DijkstrasShortestPath(source, target),target)
         
    member g.DijkstrasShortestPath(source, ?target) = 
        let dists = Dict.ofSeq [source,0.]
        let prev = Dict()
        let vs = g.Vertices
        let q = FibHeap.create()
        let visited = Hashset()
        let nodeMap = Dict()
        for v in vs do
            if v <> source then dists.Add(v,Double.MaxValue)
            let _ = prev.Add(v,None)
            let n = FibHeap.insert_data q v dists.[v]
            nodeMap.Add(v,n)
        recurse (fun stop -> stop || FibHeap.size q <= 0) (fun _ -> 
            let next = FibHeap.extract_min_data q
            if target.IsSome && next = target.Value then true
            else 
                let adjs = g.GetEdgesRaw next
                let _ = visited.Add next
                match adjs with
                | None -> false
                | Some vs -> 
                    for KeyValue(v2,weight) in vs do 
                        if not(visited.Contains v2) then 
                            let alt = dists.[next] + weight
                            if alt < dists.[v2] then 
                                dists.[v2] <- alt
                                prev.[v2] <- Some next
                                FibHeap.decrease_key q nodeMap.[v2] alt
                    false) false
        |> ignore
        dists, prev
    
    interface IWeightedGraph<'a,float> with
        member g.Vertices = g.Vertices
        member g.Edges = Array.map fst g.Edges
        member g.WeightedEdges = g.Edges
        member g.GetNeighbors n = g.GetEdges n |> Option.map (Array.map fst)
        member g.GetWeightedEdges n = g.GetEdges n
        member g.IsDirected = false
        member g.RawEdgeData() = Dict.ofSeq [|for KeyValue(k,v) in g.EdgeData -> k, Hashset(v.Keys)|]  
        member g.RawEdgeWeightData() = g.EdgeData 
        member g.AddNode v = g.InsertNode v |> ignore

        member g.RemoveNode v = g.Remove v |> ignore
        member g.InsertWeightedEdge e = g.InsertEdge e |> ignore
        member g.GetNodeNeighborCounts() = g.NodeNeighborCounts()
        member g.GetRawEdges v = g.GetEdgesRaw v  
        member g.IsWeightNormalized = weightnormalized
        member g.HasCycles = None
        member g.Ins _ = failwith "Not a directed graph"
        member g.GetNodeNeighborCount v = g.NodeNeighborCount v
        member g.ApplyToEdges f = g.ForEachEdge f
        member g.RemoveEdge(u,v) = g.RemoveEdge(u,v) |> ignore
        member g.RemoveEdge(u,v,clean) = failwith "Not a directed graph"
        member g.GetEdgeValue (a,b) = g.GetEdgeWeight (a,b)
        member g.InsertEdge(_,_)= failwith "Need weight"
        member g.ContainsEdge(u,v) = g.ContainsEdge(u,v) 

