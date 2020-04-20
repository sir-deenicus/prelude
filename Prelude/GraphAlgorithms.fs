namespace Prelude

open SimpleGraphs
open Common
open System
open SimpleDirectedGraphs


type IsCyclic = IsCyclic

type SortDirection = Ascending | Descending 

module WeightedGraph =
    let mapWeights f (g: IWeightedGraph<_, _>) =
        let d = Dict<_,_>()
        for KeyValue(n, es) in g.RawEdgeWeightData() do
            d.Add
                (n, [| for KeyValue(n2, w) in es -> n2, f w |]
                     |> Dict.ofSeq)
        if g.IsDirected then
           let wg = WeightedDirectedGraph()
           wg.InsertRange d
           Choice1Of2 wg
        else
            let wg = WeightedGraph()
            wg.InsertRange d
            Choice2Of2 wg

    let filter nodefilter edgefilter (g: IWeightedGraph<_, _>) = 
        let d = Dict<_,_>()
        for (KeyValue(n1, es)) in g.RawEdgeWeightData() do
            if nodefilter n1 then
                d.Add
                    (n1,
                     [| for KeyValue(n2, w) in es do
                         if edgefilter ((n1, n2), w) then yield (n2, w) |]
                     |> Dict.ofSeq)
        if g.IsDirected then
           let wg = WeightedDirectedGraph()
           wg.InsertRange d
           Choice1Of2 wg
        else
            let wg = WeightedGraph()
            wg.InsertRange d
            Choice2Of2 wg

type GraphAlgorithms() =
    static member isCyclic (g:IGraph<_>) =
        let tempmarks = Hashset()
        let visited = Hashset()
        let mutable errorstate = None

        let rec loop n =
            if tempmarks.Contains n then
                errorstate <- Some n
                true
            else
                if visited.Contains n then false
                else 
                    visited.Add n |> ignore
                    tempmarks.Add n |> ignore 
                    let res = 
                        g.GetNeighbors n
                        |> Option.map (Array.exists loop)
                        |> Option.defaultValue false
                    tempmarks.Remove n |> ignore 
                    res
        if g.IsDirected then
            Ok (Array.exists loop (Seq.toArray g.Vertices), errorstate)
        else Error "Not a DAG"

    static member private topologicalSortRaw isdirected vertices f getEdges = 
        let tempmarks = Hashset()  
        let completed = Hashset()
        let stack = Collections.Generic.Stack()
        let mutable error = false
        let mutable errorSource = None  

        let rec visit n =
            if not(completed.Contains n) then  
                if tempmarks.Contains n then  
                    errorSource <- Some n
                    error <- true
                else 
                    tempmarks.Add n |> ignore  
                    match getEdges n with 
                    | Some es -> 
                        for m in es do
                            if not error then visit (f m)
                    | None -> () 
                    tempmarks.Remove n |> ignore
                    completed.Add n |> ignore 
                    stack.Push n   
                
        if not isdirected then failwith "Not a Directed Graph"
        else
            let vs = Seq.toArray vertices
            let len = vs.Length
            let mutable i = 0
            while i < len && not error do 
                let v = vs.[i]
                i <- i + 1
                if not(completed.Contains v || error) then visit v 
            if error then Error (IsCyclic, errorSource) else Result.Ok (Seq.toArray stack)

    static member private transformVertices prioritizeVertices (g:IGraph<_>) =
        match prioritizeVertices with
        | None -> g.Vertices
        | Some dir -> 
            match dir with
            | Ascending ->
                g.GetNodeNeighborCounts()
                |> Array.sortBy snd
                |> Array.map fst :> 'a seq
            | Descending ->
                g.GetNodeNeighborCounts()
                |> Array.sortByDescending snd
                |> Array.map fst :> 'a seq

    static member topologicalSort (g : IWeightedGraph<_, _>, ?NodePrioritization) =
        let vertices =
            GraphAlgorithms.transformVertices NodePrioritization g
        GraphAlgorithms.topologicalSortRaw g.IsDirected vertices keyValueToKey g.GetRawEdges

    static member topologicalSort (g : IGraph<_>, ?NodePrioritization) =
        let vertices =
            GraphAlgorithms.transformVertices NodePrioritization g
        GraphAlgorithms.topologicalSortRaw g.IsDirected vertices id g.GetNeighbors

    static member minimumSpanningTree(g : IWeightedGraph<_, _>, ?domax, ?NodePrioritization) =
        let dir =
            if (defaultArg domax false) then -1.
            else 1.

        let currentCut = 
            Hashset(GraphAlgorithms.transformVertices NodePrioritization g)
        let root = currentCut |> Seq.head
        let fs = Collections.Generic.SortedSet()
        let buildTree (tree:IWeightedGraph<_,_>) =
            let _ =
                recurse (fun _ -> currentCut.Count = 0) (fun (node1, i, getnodes) ->
                    if getnodes then
                        for (KeyValue(node2, weight)) in (g.GetRawEdges node1).Value do
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
                            tree.AddNode v1
                            tree.AddNode v2
                            tree.InsertWeightedEdge(v1, v2, w)
                            let _ = currentCut.Remove v1
                            let _ = currentCut.Remove v2
                            (v2, i + 1, true)
                        else (node1, i + 1, false)) (root, 0, true)
            ()
            
        if g.IsDirected then
            let tree = WeightedDirectedGraph()
            buildTree tree
            Choice1Of2 tree
        else 
            let tree = WeightedGraph()
            buildTree tree
            Choice2Of2 tree 

    static member private extremePath comparer seedvalue (order:'a[]) (g:IWeightedGraph<'a,_>) (s:'a) =
        if not g.IsDirected then failwith "Not directed"
        let subpath = Array.skipWhile ((<>) s) order
        let d = Array.map (fun n -> n, seedvalue) subpath |> Dict.ofSeq
        d.[s] <- 0.
        let p = Dict()
        for u in Array.skipWhile ((<>) s) order do
            match g.GetRawEdges u with 
            | None -> ()
            | Some vs ->
                for KeyValue(v,w) in vs do 
                    if comparer d.[v] (d.[u] + w) then
                        d.[v] <- d.[u] + w
                        p.ExpandElseAdd v (fun _ -> u) u
        d, p

    static member readOffPath target (prevs:Dict<_,_>) = 
        let rec loop p target =
            match prevs.tryFind target with 
            | None -> p
            | Some v -> loop (v::p) v
        loop [target] target
     
    static member shortestPath (g:IWeightedGraph<'a,_>,order:'a[],s:'a) = 
        GraphAlgorithms.extremePath (>) Double.MaxValue order g s

    static member shortestPath (g:IWeightedGraph<'a,_>, order:'a[], source:'a, target :'a) = 
        GraphAlgorithms.shortestPath(g,order, source)
        |> snd |> GraphAlgorithms.readOffPath target

    static member longestPath (g:IWeightedGraph<'a,_>, order:'a[], source:'a, target :'a) = 
        GraphAlgorithms.longestPath(g,order, source)
        |> snd |> GraphAlgorithms.readOffPath target

    static member longestPath (g:IWeightedGraph<'a,_>, order:'a[],s:'a) = 
        GraphAlgorithms.extremePath (<) Double.MinValue order g s

    static member GetNeighbors(g:IGraph<_>, node, ?Degree, ?Filter) = 
        let filter = defaultArg Filter (fun _ -> true)
        let degree = defaultArg Degree 1
    
        let rec getAllNodes deg n =
            if deg = 0 then []
            else
                let nodes =
                    g.GetNeighbors n
                    |> Option.defaultValue Array.empty
                    |> Array.filter filter
                let descendants = 
                    Array.map (getAllNodes (deg - 1)) nodes
                    |> List.concat 
                    |> List.filter (fst >> Array.isEmpty >> not)
                (nodes, degree - deg + 1)::descendants //if degree = 2 and deg = 2 then 2 - 2 + 1 = 1, 2 - 1 + 1 = 2
        getAllNodes degree node
         
////////////////////////////  

module GraphVisualization =
    let rec dispTreeGeneric d maxDepth (fg: IGraph<_>) (visited: Set<_>) 
            dashes spaces node =
        match (fg.GetNeighbors node,maxDepth) with
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

    /// renders a graph that is a tree as a string.
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

    //====================================

    let fixlen maxlen s =
        if String.length s > maxlen then s.Replace(",", "\\n").Replace("/", "\\n")
        else s 
    
    let createDagreGraphGen fixlen str maxw h
        (g : IWeightedGraph<_,_>) = 
        let vs =
            g.Vertices
            |> Seq.mapi
                   (fun i v ->
                   sprintf "g.setNode(%A, {label:'%s', width:%d, height:%d});" v
                       (fixlen v) maxw h)
            |> Strings.joinToStringWith "\n"

        let es =
            g.WeightedEdges 
            |> Array.mapi
                   (fun i ((e1, e2), w) ->
                   sprintf "g.setEdge(%A, %A, {label: %A}, 'e%d')" e1 e2 (str w) i)
            |> Strings.joinToStringWith "\n"

        vs, es  

    let createDagreGraph str fixlen maxw h = createDagreGraphGen fixlen str maxw h
    
    //====================================

    type ForceGraphNodeAndName = {
        id: string;
        name: string;
        ``val`` : int
    }

    type ForceGraphNode = {
        id: string; 
        ``val`` : int
    } 

    type ForceGraphEdge = {
        source: string;
        target: string; 
    }

    type ForceGraph =
        { nodes: ForceGraphNode seq
          links: ForceGraphEdge seq }
        static member FromGraph(g: IGraph<_>) =
            { nodes =
                  g.Vertices
                  |> Seq.map (fun v ->
                      { id = v; ``val`` = 1 })
              links =
                  g.Edges
                  |> Array.map (fun (n1, n2) ->
                      { source = n1; target = n2 }) }     