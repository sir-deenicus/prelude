namespace Prelude

open SimpleGraphs
open Common
open System
open SimpleDirectedGraphs


type IsCyclic = IsCyclic

type SortDirection = Ascending | Descending 

//module WeightedGraph = 
//    let mapWeights f (g:IWeightedGraph<_,_>) =
//        let newgraph = WeightedGraph<_>()
//        for (KeyValue(n,es)) in g.EdgeData do
//            newgraph.EdgeData.Add(n, [|for KeyValue(n2,w) in es -> n2, f w |] |> Dict.ofSeq)
//        newgraph


//    let filter nodefilter edgefilter (g:WeightedGraph<_>) =
//        let newgraph = WeightedGraph<_>()
//        for (KeyValue(n1,es)) in g.EdgeData do
//            if nodefilter n1 then
//                newgraph.EdgeData.Add(n1, [|for KeyValue(n2,w) in es do if edgefilter((n1,n2),w) then yield (n2,w)|] |> Dict.ofSeq)
//        newgraph
 
type GraphAlgorithms() =
    static member private topologicalSortRaw isdirected vertices f getEdges = 
        let tempmarks = Hashset()  
        let completed = Hashset()
        let stack = Collections.Generic.Stack()
        let mutable error = false

        let rec visit n =
            if not(completed.Contains n) then  
                if tempmarks.Contains n then error <- true
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
                if not(completed.Contains v || error) then visit v 
            if error then Error IsCyclic else Result.Ok (Seq.toArray stack)

    static member private transformVertices getEdges prioritizeVertices vs =
        match prioritizeVertices with
        | None -> vs
        | Some dir ->
            let vertices =
                [ for v in vs ->
                      v,
                      getEdges v
                      |> Option.map Array.length
                      |> Option.defaultValue 0 ]
            match dir with
            | Ascending ->
                vertices
                |> List.sortBy snd
                |> List.map fst :> 'a seq
            | Descending ->
                vertices
                |> List.sortByDescending snd
                |> List.map fst :> 'a seq

    static member topologicalSort (g : IWeightedGraph<_, _>, ?prioritizeVertices) =
        let vertices =
            GraphAlgorithms.transformVertices g.GetEdges prioritizeVertices g.Vertices
        GraphAlgorithms.topologicalSortRaw g.IsDirected vertices fst g.GetWeightedEdges

    static member topologicalSort (g : IGraph<_>, ?prioritizeVertices) =
        let vertices =
            GraphAlgorithms.transformVertices g.GetEdges prioritizeVertices g.Vertices
        GraphAlgorithms.topologicalSortRaw g.IsDirected vertices id g.GetEdges

    static member minimumSpanningTree(g : IWeightedGraph<_, _>, ?domax, ?NodePrioritization) =
        let dir =
            if (defaultArg domax false) then -1.
            else 1.

        let currentCut = 
            match NodePrioritization with
             | None -> Hashset(g.Vertices)
             | Some dir -> Hashset(GraphAlgorithms.transformVertices g.GetEdges dir g.Vertices)
        let root = currentCut |> Seq.head
        let fs = Collections.Generic.SortedSet()
        let buildTree getEdges (tree:IWeightedGraph<_,_>) =
            let _, _, steps =
                recurse (fun _ -> currentCut.Count = 0) (fun (node1, i, getnodes) ->
                    if getnodes then
                        for (node2, weight) in (getEdges node1) do
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
            buildTree (g.GetWeightedEdges >> Option.get) tree
            Choice1Of2 tree
        else 
            let tree = WeightedGraph()
            buildTree (g.GetWeightedEdges >> Option.get) tree
            Choice2Of2 tree 

    static member private extremePath comparer seedvalue (order:'a[]) (g:IWeightedGraph<'a,_>) (s:'a) =
        if not g.IsDirected then failwith "Not directed"
        let subpath = Array.skipWhile ((<>) s) order
        let d = Array.map (fun n -> n, seedvalue) subpath |> Dict.ofSeq
        d.[s] <- 0.
        let p = Dict()
        for u in Array.skipWhile ((<>) s) order do
            match g.GetWeightedEdges u with 
            | None -> ()
            | Some vs ->
                for (v,w) in vs do 
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
     