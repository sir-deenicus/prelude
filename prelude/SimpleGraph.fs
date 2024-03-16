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
    abstract AddEdge : 'Node * 'Node -> unit
    abstract ContainsEdge : 'Node * 'Node -> bool option 

type IWeightedGraph<'Node, 'Weight> =
    inherit IGraph<'Node> 
    abstract WeightedEdges : (('Node * 'Node) * 'Weight) [] 
    abstract RawEdgeWeightData : unit ->  Choice<Dict<'Node, Dict<'Node, 'Weight>>, Dict<struct('Node * 'Node), 'Weight>>
    abstract GetWeightedEdges : 'Node -> ('Node * 'Weight)[] option 
    abstract GetEdgeValue : 'Node * 'Node -> 'Weight option 
    abstract AddWeightedEdge : ('Node*'Node*'Weight) -> unit 
    abstract ApplyToEdges : ('Weight -> 'Weight) -> unit
    abstract IsWeightNormalized : bool

type UndirectedGraph<'a when 'a: equality and 'a: comparison>() = 
    let mutable edges = Dict<'a,'a Hashset>()
    member g.EdgeData = edges   
    
    member g.AddNode(v) =
        let contained = edges.ContainsKey v
        if not contained then edges.Add(v,Hashset())
        contained

    member g.FromDictionary(edgesraw) =
       edges.Clear()
       edges <- edgesraw

    member g.AddEdge(v0,v1) = 
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
  
            
    static member fromEdges (edges:_) =
        let g = new UndirectedGraph<'a>()
        g.AddEdges edges
        g 

    member g.Clear() = edges.Clear()

    member g.AddEdges(edges) =
        for (v1, v2) in edges do g.AddEdge(v1, v2) |> ignore

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
        member g.AddNode v = g.AddNode v |> ignore 

        member g.RemoveNode v = g.Remove v |> ignore

        member g.AddEdge(u,v) = g.AddEdge(u,v) |> ignore
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
type WeightedGraph<'a when 'a: equality and 'a: comparison>() =
    let mutable edges = Dict<'a, Hashset<'a>>()
    let mutable edgeWeights = Dict<struct ('a * 'a),float>()
      
    member g.EdgeData = edges 

    member g.ForEachEdge f =
        for (v, vs) in keyValueSeqtoPairArray edges do
            for v2 in vs do 
                let (n1, n2) = Pair.lessToLeft(v, v2)
                let w = edgeWeights[struct (n1, n2)]
                edgeWeights[struct (n1, n2)] <- f w

    member g.RemoveVerticesWhere f =
        let keys = g.Vertices
        for k in keys do
            if f k then
                g.Remove k |> ignore
                 
    member g.RemoveEdgesWhere f =
        for (v, vs) in keyValueSeqtoPairArray edges do
            for v2 in vs do
                let (n1, n2) = Pair.lessToLeft(v, v2)
                let w = edgeWeights[struct (n1, n2)]
                if f ((v,v2),w) then g.RemoveEdge(v,v2) |> ignore

    member g.FromDictionary(edgesDict, edgeWeightsDict) =
        edges.Clear()
        edges <- edgesDict
        edgeWeights.Clear()
        edgeWeights <- edgeWeightsDict
        
    member g.AddNode(s: 'a) =
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s,Hashset())
        contained
    
    member g.Remove(v: 'a) =
        match (edges.TryFind v) with //find edges
        | None -> false
        | Some edgelist -> //nodes connected to
            for v2 in edgelist do 
                let (n1, n2) = Pair.lessToLeft(v, v2)
                edgeWeights.Remove(struct (n1, n2)) |> ignore
                edges[v2].Remove(v) |> ignore
                if edges[v2].Count = 0 then ignore(edges.Remove v2)
            edges.Remove v

    member g.AddEdge(v0,v1,w, ?overwriteWeight) =
        let overwrite = defaultArg overwriteWeight true
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

        let (n1, n2) = Pair.lessToLeft(v0, v1)
        match edgeWeights.TryFind(struct (n1, n2)) with
        | Some _ when overwrite -> edgeWeights.[struct (n1, n2)] <- w
        | Some _  -> ()
        | None -> edgeWeights.Add(struct (n1, n2), w) |> ignore

        in0, in1

    member g.AddEdges edges =
        for (v1,v2,w) in edges do
            g.AddEdge(v1,v2,w) |> ignore

    static member fromEdges edges =
        let g = new WeightedGraph<'a>()
        g.AddEdges edges
        g

    member g.RemoveEdge(v1,v2) =
        maybe {
            let! elist0 = edges.TryFind v1
            let! elist1 = edges.TryFind v2
            let in0 = elist0.Remove v2
            let in1 = elist1.Remove v1
            let (n1, n2) = Pair.lessToLeft(v1, v2)
            let _ = edgeWeights.Remove(struct (n1, n2))
            let _ = if elist0.Count = 0 then edges.Remove v1 |> ignore
            let _ = if elist1.Count = 0 then edges.Remove v2 |> ignore
            return (in0,in1)
        }

    member g.ContainsVertex v = edges.ContainsKey v
    
    member g.AdjustWeight (v1,v2, f) =
        let vertless,vertHigher = Pair.lessToLeft(v1,v2) 
        maybe {       
            match edgeWeights.TryFind(struct (vertless,vertHigher)) with
            | Some w -> 
                edgeWeights.[struct (vertless,vertHigher)] <- f w
                return true
            | None ->  return false
        }
        
    member __.EdgeWeights = edgeWeights

    member g.GetEdgeWeight (v1, v2) =
        let vertless, vertHigher = Pair.lessToLeft(v1, v2)
        edgeWeights.TryFind(struct (vertless, vertHigher))

    member private g.getWeight (v1, v2) =
        let vertless, vertHigher = Pair.lessToLeft(v1, v2)
        edgeWeights[struct (vertless, vertHigher)]

    /// <summary>
    /// Sends a message `state` to neighbors of node "v" via f, which takes a message, the current node and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.
    /// </summary>
    /// <param name="state">The initial state or message to be passed to the neighbors of node "v". This state can be updated with each recursive call based on the return value of function "f".</param>
    /// <param name="v">The node to send the message from</param>
    /// <param name="f">The function to apply to the message, f takes a message, the current node and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.</param>
    member g.SendMessageToNeighbors (state:'b, v, f) =
        let nodes = g.GetEdges v 
        for (node, weight) in nodes do
            let msg', terminate = f state (v, node, weight)
            if terminate then () else g.SendMessageToNeighbors (msg', node, f)


    /// <summary>
    /// Asynchronously sends a message `state` to neighbors of node "v" via f, which takes a message, the current node and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.
    /// </summary>
    /// <param name="state">The initial state or message to be passed to the neighbors of node "v". This state can be updated with each recursive call based on the return value of function "f".</param>
    /// <param name="v">The node to send the message from</param>
    /// <param name="f">The function to apply to the message, f takes a message, the current node and the current targeted node and returns a new message and a boolean indicating whether to terminate the message passing.</param>
    member g.SendMessageToNeighborsAsync (state:'b, v, f) =
        async {
            let nodes = g.GetEdges v 
            for (node, weight) in nodes do
                let! msg', terminate = f state (v, node, weight)
                if terminate then () 
                do! g.SendMessageToNeighborsAsync (msg', node, f)
                return ()
        }  

    member g.ContainsEdge (v1, v2) =
        let (n1,n2) = Pair.lessToLeft (v1, v2)
        edgeWeights.ContainsKey (struct (n1,n2)) 
    
    member g.GetEdges v : ('a * float)[]  = 
        [| match edges.TryFind v with
            | Some vs ->
                for v2 in vs do 
                    let w = g.getWeight(v,v2)
                    yield v2, w
            | None -> ()|]
    
    member g.Edges = edgeWeights
    
    member g.OrderedEdges =
        let sorted = Collections.Generic.SortedSet() 
        for KeyValue(v1,vs) in g.EdgeData do
            for v2 in vs do 
                let w = g.getWeight(v1,v2)
                sorted.Add (WeightedNode(w, Pair.lessToLeft(v1,v2)))
                |> ignore
        [|for v in sorted -> v.Node, v.Weight|]
    
    member g.NodeNeighborCounts() = 
        [|for KeyValue(v1,vs) in g.EdgeData -> v1, vs.Count |] 

    member g.NodeNeighborCount v = 
        Option.map (fun (vs:Hashset<_>) -> vs.Count) (edges.TryFind v)
    
    member g.Vertices = [|for k in edges.Keys -> k|] 

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
                    for node2 in edges.[node1] do
                        if currentCut.Contains node2 
                           || currentCut.Contains node1 then 
                            let weight = g.getWeight(node1,node2)
                            ignore(fs.Add(WeightPair(weight * dir,node1,node2)))
                if fs.Count = 0 then (currentCut |> Seq.head, i + 1, true)
                else 
                    let v1,v2,w,next =
                        let minel = fs.Min
                        minel.VertX, minel.VertY, minel.Weight, minel
                    
                    let _ = fs.Remove next
                    if (currentCut.Contains v1 || currentCut.Contains v2) then 
                        let _ = tree.AddNode v1
                        let _ = tree.AddNode v2
                        let _ = tree.AddEdge(v1,v2,w)
                        let _ = currentCut.Remove v1
                        let _ = currentCut.Remove v2
                        (v2,i + 1,true)
                    else (node1,i + 1,false)) (root,0,true)
        tree 

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
                let nextEdges = g.GetEdges next
                let _ = visited.Add next 
                for (v2, weight) in nextEdges do 
                    if not(visited.Contains v2) then 
                        let newDistance = dists.[next] + weight
                        if newDistance < dists.[v2] then 
                            dists.[v2] <- newDistance
                            prev.[v2] <- Some next
                            FibHeap.decrease_key q nodeMap.[v2] newDistance
                false) false
        |> ignore
        dists, prev
    
    interface IWeightedGraph<'a,float> with
        member g.Vertices = g.Vertices
        member g.Edges = 
            [| for KeyValue(k,_) in g.EdgeWeights do
                let struct (v1,v2) = k
                (v1,v2) |]

        member g.WeightedEdges = 
            [| for KeyValue(k,w) in g.EdgeWeights do
                let struct (v1,v2) = k
                (v1,v2),w |]

        member g.GetNeighbors n = g.GetEdges n |> Array.map fst |> Some
        member g.GetWeightedEdges n = g.GetEdges n |> Some
        member g.IsDirected = false
        member g.RawEdgeData() = g.EdgeData
        member g.RawEdgeWeightData() = Choice2Of2 (g.EdgeWeights)
        member g.AddNode v = g.AddNode v |> ignore
        member g.RemoveNode v = g.Remove v |> ignore
        member g.AddWeightedEdge ((u,v,w)) = g.AddEdge(u,v,w) |> ignore
        member g.GetNodeNeighborCounts() = g.NodeNeighborCounts()  
        member g.IsWeightNormalized = false
        member g.HasCycles = None
        member g.Ins _ = failwith "Not a directed graph"
        member g.GetNodeNeighborCount v = g.NodeNeighborCount v
        member g.ApplyToEdges f = g.ForEachEdge f
        member g.RemoveEdge(u,v) = g.RemoveEdge(u,v) |> ignore
        member g.RemoveEdge(u,v,clean) = failwith "Not a directed graph"
        member g.GetEdgeValue (a,b) = g.GetEdgeWeight (a,b)
        member g.AddEdge(_,_) = failwith "Need weight"
        member g.ContainsEdge(u,v) = g.ContainsEdge(u,v) |> Some 

