module Prelude.SimpleGraphs

open Prelude.Common
open System
open Prelude.Collections.FibonacciHeap

type IGraph<'Node> =
    abstract Vertices : seq<'Node> 
    abstract Edges : ('Node * 'Node) [] 
    abstract IsDirected : bool  
    abstract RawEdgeData : unit -> Dict<'Node, Hashset<'Node>>
    abstract AddNode : 'Node -> unit
    abstract GetNeighbors : 'Node -> 'Node [] option
    abstract GetNodeNeighborCounts : unit -> ('Node * int) [] 
    abstract GetNodeNeighborCount : 'Node -> int option
    abstract HasCycles : bool option 
     
type IWeightedGraph<'Node, 'Weight> =
    inherit IGraph<'Node> 
    abstract WeightedEdges : (('Node * 'Node) * 'Weight) [] 
    abstract RawEdgeWeightData : unit -> Dict<'Node, Dict<'Node, 'Weight>>
    abstract GetWeightedEdges : 'Node -> ('Node * 'Weight) [] option 
    abstract InsertWeightedEdge : ('Node*'Node*'Weight) -> unit
    abstract GetRawEdges : 'Node -> Dict<'Node,'Weight> option
    abstract IsWeightNormalized : bool
    abstract ApplyToEdges : ('Weight -> 'Weight) -> unit

type UndirectedGraph<'a when 'a: equality and 'a: comparison>() = 
    let edges = Dict<'a,'a Hashset>()
    member g.EdgeData = edges   
    
    member g.InsertVertex(v) =
        let contained = edges.ContainsKey v
        if not contained then edges.Add(v,Hashset())
        contained

    member g.Vertices = [|for KeyValue(k,_) in g.EdgeData -> k|] 

    member g.Edges =
        [|for KeyValue(v1,vs) in g.EdgeData do
            for v2 in vs -> lessToLeft(v1,v2) |] 
        |> Hashset
        |> Seq.toArray

    member g.Remove(v: 'a) =
        match (edges.tryFind v) with
        | None -> false
        | Some elist -> 
            for v2 in elist do 
                edges.[v2].Remove(v) |> ignore
                if edges.[v2].Count = 0 then ignore(edges.Remove v2)
            edges.Remove v
    
    member g.RemoveEdge(v0,v1) =
        maybe {
            let! elist0 = edges.tryFind v0
            let! elist1 = edges.tryFind v1
            let in0 = elist0.Remove v1
            let in1 = elist1.Remove v0
            let _ = if elist0.Count = 0 then edges.Remove v0 |> ignore
            let _ = if elist1.Count = 0 then edges.Remove v1 |> ignore
            return (in0,in1)
        }
    member g.InsertEdge(v0,v1) =
        maybe {
            let! elist0 = edges.tryFind v0
            let! elist1 = edges.tryFind v1
            let in0 = elist0.Add v1
            let in1 = elist1.Add v0
            return (in0,in1)
        }
    
    member g.ContainsVertex v = edges.ContainsKey v

    member g.ContainsEdge (v1,v2) = maybe {
        let! elist0 = edges.tryFind v1
        return (elist0.Contains v2) }

    member g.GetEdges v = maybe {
        let! elist = edges.tryFind v
        return elist |> Seq.toArray }

    member g.NodeNeighborCounts () = 
        [|for KeyValue(v1,vs) in g.EdgeData -> v1, vs.Count |]  
    
    member g.NodeNeighborCount v = 
        Option.map (fun (vs:Hashset<_>) -> vs.Count) (edges.tryFind v)
         
    interface IGraph<'a> with
        member g.Vertices = seq g.Vertices
        member g.Edges = g.Edges  
        member g.GetNeighbors n = g.GetEdges n
        member g.IsDirected = false  
        member g.RawEdgeData() = g.EdgeData 
        member g.HasCycles = None
        member g.GetNodeNeighborCounts() = g.NodeNeighborCounts()
        member g.GetNodeNeighborCount v = g.NodeNeighborCount v
        member g.AddNode v = g.InsertVertex v |> ignore 
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

[<Struct;CustomComparison;CustomEquality>]
type WeightedNode<'a when 'a: comparison> =
    val Weight: float
    val Node: 'a    
    new(w: float,x) =
        {Weight = w
         Node = x}    
    override g.ToString() = (string g.Weight) + ", " + (g.Node.ToString())    
    interface IEquatable<WeightedNode<'a>> with
        member this.Equals(other) =
            this.Weight = other.Weight && this.Node = other.Node    
    interface IComparable<WeightedNode<'a>> with
        member this.CompareTo(other) =
            if this.Weight = other.Weight then compare this.Node other.Node
            else this.Weight.CompareTo(other.Weight)

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
                if not (f ((v,v2),w)) then g.RemoveEdge(v,v2) |> ignore

    member g.InsertRange(edgesRaw,?edgeweights) =
        edges.Clear()
        edges <- edgesRaw
        edgeWeights.Clear()
        if edgeweights.IsSome then
           edgeWeights <- edgeweights.Value
        
    member g.InsertVertex(s: 'a) =
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s,Dict())
        contained
    
    member g.Remove(v: 'a) =
        match (edges.tryFind v) with //find edges
        | None -> false
        | Some edgelist -> //nodes connected to
            for KeyValue(connectedNode,_) in edgelist do 
                if dofastweights then
                    let vertless,vertHigher = lessToLeft(v,connectedNode)
                    let node = struct (vertless,vertHigher)
                    match edgeWeights.tryFind node with
                    | Some _ -> ignore (edgeWeights.Remove node)
                    | _ -> ()
                edges.[connectedNode].Remove(v) |> ignore
                if edges.[connectedNode].Count = 0 then ignore(edges.Remove connectedNode)
            edges.Remove v
    
    member g.InsertEdge(v0,v1,w) =
        let node1,node2 = lessToLeft(v0,v1)
        if not dofastweights || not(edgeWeights.ContainsKey(struct (node1,node2))) then 
            maybe {
                let! connectedsToNode1 = edges.tryFind node1
                if connectedsToNode1.ContainsKey node2 then 
                   return false
                else
                    let! node2connecteds = edges.tryFind node2
                    connectedsToNode1.Add(node2, w)
                    node2connecteds.Add(node1, w)
                
                    let _ =
                        if dofastweights then 
                            ignore(edgeWeights.ExpandElseAdd (struct (node1,node2)) id w) 
                    return true
            }
        else None

    member g.RemoveEdge(v1,v2) =
        maybe {
            let! connectedToNode1 = edges.tryFind v1 
            let! node2connecteds = edges.tryFind v2
            let r = connectedToNode1.Remove(v2)
            let _ = node2connecteds.Remove(v1)
            let _ = if connectedToNode1.Count = 0 then edges.Remove v1 |> ignore
            let _ = if node2connecteds.Count = 0 then edges.Remove v2 |> ignore

            let _ =
                if dofastweights then  
                    let node1,node2 = lessToLeft(v1,v2) 
                    ignore(edgeWeights.Remove (struct (node1,node2)))
            return r
        } 

    member g.ContainsVertex v = edges.ContainsKey v
    
    member g.AdjustWeight f (v1,v2) =
        let vertless,vertHigher = lessToLeft(v1,v2)
        if dofastweights then 
            maybe {       
                match edgeWeights.tryFind(struct (vertless,vertHigher)) with
                | Some w -> 
                    edgeWeights.[struct (vertless,vertHigher)] <- f w
                    return true
                | None -> return! g.InsertEdge(v1,v2,f 0.)   
            }
        else  
            match edges.tryFind v1 with
            | Some elistV1 ->
                match edges.tryFind v2 with 
                | Some elistV2 ->  
                    match elistV1.tryFind v2 with 
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
                let a,b = lessToLeft(v1,v2)
                let! w = edgeWeights.tryFind(struct (a,b))
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
        return elist}
    
    member g.GetEdges v = Option.map keyValueSeqtoPairArray (g.GetEdgesRaw v)
    
    member g.Edges = 
        [|for KeyValue(v1,vs) in g.EdgeData do
            for KeyValue(v2,w) in vs -> lessToLeft (v1,v2), w |]
        |> Hashset
        |> Seq.toArray 
    
    member g.NodeNeighborCounts() = 
        [|for KeyValue(v1,vs) in g.EdgeData -> v1, vs.Count |] 

    member g.NodeNeighborCount v = 
        Option.map (fun (vs:Dict<_,_>) -> vs.Count) (edges.tryFind v)

    member g.OrderedEdges =
        let sorted = Collections.Generic.SortedSet() 
        for KeyValue(v1,vs) in g.EdgeData do
            for KeyValue(v2,w) in vs do 
                sorted.Add (WeightedNode(w, lessToLeft(v1,v2)))
                |> ignore
        [|for v in sorted -> v.Node, v.Weight|]
    
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
                        let _ = tree.InsertVertex v1
                        let _ = tree.InsertVertex v2
                        let _ = tree.InsertEdge(v1,v2,w)
                        let _ = currentCut.Remove v1
                        let _ = currentCut.Remove v2
                        (v2,i + 1,true)
                    else (node1,i + 1,false)) (root,0,true)
        tree
    
    member g.Vertices = [|for k in edges.Keys -> k|] 

    member g.extractShortestPath f ((dists: Dict<_,_>, prev: Dict<_,_>), target) =
        recurse (fst >> Option.isNone) 
            (fun (path,l) ->
                match path with 
                | Some p -> prev.getOrDefault None p,f(p,dists.[p]) :: l 
                | _ -> failwith "no path") 
            (Some target,[])
            |> snd

    member g.shortestPaths source = 
        let paths = g.dijkstrasShortestPath source
        g.Vertices 
        |> Array.map (fun v -> g.extractShortestPath id (paths, v))

    member g.shortestPath (source, target) = g.extractShortestPath fst (g.dijkstrasShortestPath(source, target),target)
         
    member g.dijkstrasShortestPath(source, ?target) = 
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
        member g.Vertices = seq g.Vertices
        member g.Edges = Array.map fst g.Edges
        member g.WeightedEdges = g.Edges
        member g.GetNeighbors n = g.GetEdges n |> Option.map (Array.map fst)
        member g.GetWeightedEdges n = g.GetEdges n
        member g.IsDirected = false
        member g.RawEdgeData() = Dict.ofSeq [|for KeyValue(k,v) in g.EdgeData -> k, Hashset(v.Keys)|]  
        member g.RawEdgeWeightData() = g.EdgeData 
        member g.AddNode v = g.InsertVertex v |> ignore
        member g.InsertWeightedEdge e = g.InsertEdge e |> ignore
        member g.GetNodeNeighborCounts() = g.NodeNeighborCounts()
        member g.GetRawEdges v = g.GetEdgesRaw v  
        member g.IsWeightNormalized = weightnormalized
        member g.HasCycles = None
        member g.GetNodeNeighborCount v = g.NodeNeighborCount v
        member g.ApplyToEdges f = g.ForEachEdge f

