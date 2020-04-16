module Prelude.SimpleGraphs

open Prelude.Common
open System
open Prelude.Collections.FibonacciHeap

type IGraph<'Node, 'Edge, 'Weight> =
    abstract Vertices : seq<'Node> 
    abstract Edges : ('Node * 'Node) []
    abstract WeightedEdges : (('Node * 'Node) * 'Weight) [] 
    abstract GetEdges : 'Node -> 'Edge [] option
    abstract IsDirected : bool 

type UndirectedGraph<'a when 'a: equality and 'a: comparison>() = 
    let edges = Dict<'a,'a Hashset>()
    member g.EdgeData = edges  
    
    member g.InsertVertex(v: 'a) =
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
            for v2 in elist do edges.[v2].Remove(v) |> ignore
            edges.Remove v
    
    member g.InsertEdge(v0,v1) =
        maybe {
            let! elist0 = edges.tryFind v0
            let! elist1 = edges.tryFind v1
            let in0 = elist0.Add v1
            let in1 = elist1.Add v0
            return (in0,in1)
        }
    
    member g.ContainsVertex v = edges.ContainsKey v

    member g.ContainsEdge v1 v2 = maybe {
        let! elist0 = edges.tryFind v1
        return (elist0.Contains v2) }

    member g.GetEdges v = maybe {
        let! elist = edges.tryFind v
        return elist |> Seq.toArray }
                                   
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

//===================================================

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
type WeightedGraph<'a when 'a: equality and 'a: comparison>(?trackweights) =
    let mutable edges = Dict<'a,Hashset<WeightedNode<'a>>>()
    let mutable edgeWeights =
        Dict<struct ('a * 'a),float>(HashIdentity.Structural)
    let track_weights = defaultArg trackweights false
     
    member g.EdgeData = edges

    member g.ForEach f =
        let keys = g.Vertices
        for k in keys do
            edges.[k] <- f edges.[k]
    
    member g.Filter f =
        let keys = g.Vertices
        for k in keys do
            if f keys then
                g.Remove k |> ignore
                 
    member g.InsertRange(es,ews) =
        edges.Clear()
        edges <- es
        edgeWeights.Clear()
        edgeWeights <- ews
    
    member g.InsertVertex(s: 'a) =
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s,Hashset())
        contained
    
    member g.Remove(v: 'a) =
        match (edges.tryFind v) with //find edges
        | None -> false
        | Some edgelist -> //nodes connected to
            for connectedNode in edgelist do 
                let vertless,vertHigher = lessToLeft(v,connectedNode.Node)
                let node = struct (vertless,vertHigher)
                match edgeWeights.tryFind node with
                | Some _ -> ignore <| edgeWeights.Remove node
                | _ -> ()
                edges.[connectedNode.Node].Remove(v) |> ignore
            edges.Remove v
    
    member g.InsertEdge(v0,v1,w) =
        let x,y = lessToLeft(v0,v1)
        if not track_weights || not(edgeWeights.ContainsKey(struct (x,y))) then 
            maybe {
                let! elist0 = edges.tryFind v0
                let! elist1 = edges.tryFind v1
                let added1 = elist0.Add(WeightedNode(w,v1))
                let added2 = elist1.Add(WeightedNode(w,v0))
                
                let _ =
                    if track_weights then 
                        ignore <| edgeWeights.ExpandElseAdd (struct (x,y)) id w 
                return (added1,added2)
            }
        else None
    
    member g.ContainsVertex v = edges.ContainsKey v
    
    member g.AdjustWeight f v1 v2 =
        let vertless,vertHigher = lessToLeft(v1,v2)
        if track_weights then 
            maybe {      // get current weight
                let! w = match edgeWeights.tryFind(struct (vertless,vertHigher)) with
                         | Some _ as w -> w
                         | _ -> 
                             g.InsertEdge(v1,v2,0.) |> ignore
                             Some 0.

                let elistV1 = edges.[v1]
                let elistV2 = edges.[v2] 

                let currentV2 = WeightedNode(w,v2) 
                elistV1.Remove(currentV2) |> ignore

                let w' = f w
                let adj1 = elistV1.Add(WeightedNode(w',v2))
                elistV2.Remove(WeightedNode(w,v1)) |> ignore
                let adj2 = elistV2.Add(WeightedNode(w',v1))
                edgeWeights.[struct (vertless,vertHigher)] <- w'
                return (adj1,adj2)
            }
        else None
    
    member __.EdgeWeights = edgeWeights
    
    member g.GetEdgeWeight v1 v2 =
        let a,b = lessToLeft(v1,v2)
        maybe {let! w = edgeWeights.tryFind(struct (a,b))
               return w }
    
    member g.ContainsEdge v1 v2 = maybe {
        let! elist0 = edges.tryFind v1
        return (elist0.Contains v2) }
    
    member g.GetEdgesRaw v = maybe {
        let! elist = edges.tryFind v
        return elist}
    
    member g.GetEdges v = maybe {
        let! elist = edges.tryFind v
        return [|for e in elist -> e.Node, e.Weight |]}
    
    member g.Edges = 
        [|for KeyValue(v1,vs) in g.EdgeData do
            for v2 in vs -> lessToLeft (v1,v2.Node), v2.Weight |]
        |> Hashset
        |> Seq.toArray 
    
    member g.OrderedEdges =
        let sorted = Collections.Generic.SortedSet() 
        for KeyValue(v1,vs) in g.EdgeData do
            for v2 in vs do 
                sorted.Add (WeightedNode(v2.Weight, lessToLeft(v1,v2.Node)))
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
                    for node2 in edges.[node1] do
                        if currentCut.Contains node2.Node 
                           || currentCut.Contains node1 then 
                            ignore(fs.Add(WeightPair(node2.Weight * dir,node1,node2.Node)))
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
    
    //member g.dijkstra_full source =
    //    let dists = Dict.ofSeq [source,0.]
    //    let prev = Dict()
    //    let vs = g.Vertices
    //    let q = FibHeap.create()
    //    let visited = Hashset()
    //    let nodeMap = Dict()
    //    for v in vs do
    //        if v <> source then dists.Add(v,Double.MaxValue)
    //        let _ = prev.Add(v,None)
    //        let n = FibHeap.insert_data q v dists.[v]
    //        nodeMap.Add(v,n)
    //    recurse (fun stop -> stop || FibHeap.size q <= 0) (fun _ -> 
    //        let next = FibHeap.extract_min_data q
    //        let adjs = g.GetEdgesRaw next
    //        let _ = visited.Add next
    //        match adjs with
    //        | None -> false
    //        | Some vs -> 
    //            for v2 in vs do 
    //                if not(visited.Contains v2.Node) then 
    //                    let alt = dists.[next] + v2.Weight
    //                    if alt < dists.[v2.Node] then 
    //                        dists.[v2.Node] <- alt
    //                        prev.[v2.Node] <- Some next
    //                        FibHeap.decrease_key q nodeMap.[v2.Node] alt
    //            false) (false)
    //    |> ignore
    //    dists,prev    
    //member g.shortest_dists (dists: Dict<_,_>) (prev: Dict<_,_>) target =
    //    recurse (fst >> Option.isNone) 
    //        (fun ((Some p),l) -> prev.getOrDefault None p,(p,dists.[p]) :: l) 
    //        (Some target,[])
    //member g.shortest_path (prev: Dict<_,_>) target =
    //    recurse (fst >> Option.isNone) 
    //        (fun ((Some p),l) -> prev.getOrDefault None p,p :: l) 
    //        (Some target,[])
    member g.dijkstra(source, ?target) = 
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
                    for v2 in vs do 
                        if not(visited.Contains v2.Node) then 
                            let alt = dists.[next] + v2.Weight
                            if alt < dists.[v2.Node] then 
                                dists.[v2.Node] <- alt
                                prev.[v2.Node] <- Some next
                                FibHeap.decrease_key q nodeMap.[v2.Node] alt
                    false) false
        |> ignore
        recurse (fst >> Option.isNone) 
            (fun ((Some p),l) -> prev.getOrDefault None p,p :: l) 
            (Some target,[]) 
    
    interface IGraph<'a,'a * float,float> with
        member g.Vertices = seq g.Vertices
        member g.Edges = Array.map fst g.Edges
        member g.WeightedEdges = g.Edges
        member g.GetEdges n = g.GetEdges n

module WeightedGraph = 
    let map nodemap edgemap (g:WeightedGraph<_>) =
        let newgraph = WeightedGraph<_>()
        for (KeyValue(n,es)) in g.EdgeData do
            newgraph.EdgeData.Add(nodemap n, edgemap es)
        newgraph
    let filter nodefilter edgefilter (g:WeightedGraph<_>) =
        let newgraph = WeightedGraph<_>()
        for (KeyValue(n,es)) in g.EdgeData do
            if nodefilter n then
                newgraph.EdgeData.Add(n, es |> Seq.filter edgefilter |> Hashset)
        newgraph

////////////////////////////  

let rec dispTreeGeneric d maxDepth (fg: IGraph<_,_,_>) (visited: Set<string>) 
        dashes spaces node =
    match (fg.GetEdges node,maxDepth) with
    | None,_ -> node
    | _,Some n when d >= n -> node
    | Some edges,_ -> 
        let children =
            edges 
            |> Array.filterMap (visited.Contains >> not) 
                   (fun e -> 
                   spaces + "|" 
                   + (dispTreeGeneric (d + 1) maxDepth fg (visited.Add node) 
                          (dashes + "-") (spaces + "|  ") e))
        dashes + node + "\n" 
        + (children |> Strings.joinToStringWith Strings.newLine)

///renders a graph that is a tree as a string.
let dispStringTree d maxDepth g node =
    dispTreeGeneric d maxDepth g Set.empty "-" " " node 


let disp (template:string) isleftright svgid w h (vs,es) =
    let rankdir = if isleftright then """rankdir: "LR",""" else ""
    template
        .Replace("__EDGES_HERE__", es)
        .Replace("__NODES_HERE__",vs)
        .Replace("svgid", svgid)
        .Replace("svgwidth", string w)
        .Replace("svgheight", string h)
        .Replace("__RANK_DIR__", rankdir)

let fixlen maxlen s =
    if String.length s > maxlen then s.Replace(",", "\\n").Replace("/", "\\n")
    else s 
    
let createDagreGraphGen fixlen str maxw h
    (g : IGraph<_,_,_>) = 
    let vs =
        g.Vertices
        |> Seq.map
               (fun v ->
               sprintf "g.setNode(%A, {label:'%s', width:%d, height:%d});" v
                   (fixlen v) (min maxw ((String.length v) * 8)) h)
        |> Strings.joinToStringWith "\n"

    let es =
        g.WeightedEdges 
        |> Array.mapi
               (fun i ((e1, e2), w) ->
               sprintf "g.setEdge(%A, %A, {label: %A}, 'e%d')" e1 e2 (str w) i)
        |> Strings.joinToStringWith "\n"

    vs, es  
 
let createDagreGraph str fixlen maxw h = createDagreGraphGen fixlen str maxw h
       
type VizGraphNodeAndName = {
    id: string;
    name: string;
    ``val`` : int
}

type VizGraphNode = {
    id: string; 
    ``val`` : int
} 

type VizGraphEdge = {
    source: string;
    target: string; 
}

type VizGraph =
    { nodes: VizGraphNode seq
      links: VizGraphEdge seq }
    static member FromGraph(g: IGraph<_, _, _>) =
        { nodes =
              g.Vertices
              |> Seq.map (fun v ->
                  { id = v; ``val`` = 1 })
          links =
              g.WeightedEdges
              |> Array.map (fun ((n1, n2), w) ->
                  { source = n1; target = n2 }) }
