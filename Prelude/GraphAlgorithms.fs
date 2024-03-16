namespace Prelude

open SimpleGraphs
open Common
open System
open SimpleDirectedGraphs
open Math
open Prelude.Collections.FibonacciHeap

  
type DAGError<'a> = NotaDAG | IsCyclic of 'a 

type NotCyclic = NotCyclic

type SortDirection = Ascending | Descending 
   
module WeightedGraph =
    let getWeightsDict f (g: IWeightedGraph<_, _>) =
        let d = Dict<_,_>()
        for KeyValue(n, es) in g.RawEdgeWeightData() do
            d.Add
                (n, [| for KeyValue(n2, w) in es -> n2, f w |]
                     |> Dict.ofSeq)
        d

    let getWeightsFilteredDict nodefilter edgefilter (g: IWeightedGraph<_, _>) = 
        let d = Dict<_,_>()
        for (KeyValue(n1, es)) in g.RawEdgeWeightData() do
            if nodefilter n1 then
                d.Add
                    (n1,
                     [| for KeyValue(n2, w) in es do
                         if edgefilter ((n1, n2), w) then yield (n2, w) |]
                     |> Dict.ofSeq)
        d

    let mapWeights f (g: IWeightedGraph<_, _>) =
        let d = getWeightsDict f g
        let wg = WeightedGraph()
        wg.InsertRange d
        wg

    let filter nodefilter edgefilter (g: IWeightedGraph<_, _>) = 
        let d = getWeightsFilteredDict nodefilter edgefilter g 
        let wg = WeightedGraph()
        wg.InsertRange d
        wg  
    
    ///for example, given a list [a,b,c] if these are connected in the source graph, they will be connected in the target graph
    ///which should ideally be empty.
    let fromVertexList (source:IWeightedGraph<_,_>, target:IWeightedGraph<_,_>) (vs : _ seq) =
        for a in vs do 
            for b in vs do
            match source.GetEdgeValue(a,b) with 
            | None -> ()
            | Some w -> 
                target.AddNode a; target.AddNode b
                target.InsertWeightedEdge(a,b,w) 

    let ofEdgeList (g:IWeightedGraph<_,_>) (es : _ seq) =
        for (a,b) in es do 
            match g.GetEdgeValue(a,b) with
            | None -> () 
            | Some w -> 
                g.AddNode a; g.AddNode b
                g.InsertWeightedEdge(a,b,w) 

    let ofWeightedEdgeList (g:IWeightedGraph<_,_>) (es : _ seq) =
        for (a,b,_) as e in es do 
            g.AddNode a
            g.AddNode b
            g.InsertWeightedEdge e

module WeightedDirectedGraph =
    let mapWeights f (g: IWeightedGraph<_, _>) =
        let d = WeightedGraph.getWeightsDict f g
        
        let wg = WeightedDirectedGraph()
        wg.InsertRange d
        wg 

    let filter nodefilter edgefilter (g: IWeightedGraph<_, _>) = 
        let d = WeightedGraph.getWeightsFilteredDict nodefilter edgefilter g 
        let wg = WeightedDirectedGraph()
        wg.InsertRange d
        wg  
   


module DirectedMultiGraph= 
    
    let inline mergeNodes reduceNodeWeights (g:DirectedMultiGraph<_, _>) =
        Array.iter (g.ModifyNodeEdges reduceNodeWeights) g.Vertices

    let mapWeights f (g: DirectedMultiGraph<_, _>) = 
        let d = Dict<_,_>() 
        for KeyValue(n, es) in g.EdgeData do
            d.Add
                (n, [| for KeyValue(n2, ws) in es -> n2, ResizeArray(Seq.map f ws) |]
                     |> Dict.ofSeq) 
                      
        let wg = DirectedMultiGraph()
        wg.InsertRange d
        wg 
        
    let getWeightsFilteredDict nodefilter edgefilter (g: DirectedMultiGraph<_, _>) = 
        let d = Dict<_,_>()
        for (KeyValue(n1, es)) in g.EdgeData do
            if nodefilter n1 then
                d.Add
                    (n1,
                     [| for KeyValue(n2, w) in es do
                         if edgefilter ((n1, n2), w) then yield (n2, w) |]
                     |> Dict.ofSeq)
        d

    let filter nodefilter edgefilter (g: DirectedMultiGraph<_, _>) = 
        let d = getWeightsFilteredDict nodefilter edgefilter g 
        let wg = DirectedMultiGraph()
        wg.InsertRange d
        wg  

    let filterNodes nodefilter (g: DirectedMultiGraph<_, _>) = 
        for v in g.Vertices do
            if not (nodefilter v) then
                g.Remove(v) |> ignore
        g  
 
            
module Graph = 
    let createSmallWorldGraph (n:int) (k:int) (p:float) =
        let g = Prelude.SimpleGraphs.UndirectedGraph()
        let nodes = [|0..n-1|]

        for i in 0..n-1 do
            g.AddNode(i) |> ignore
            // Connect each vertex to its k nearest neighbors on either side
            for j in 1..k/2 do
                g.AddEdge(i, (i+j) % n) |> ignore
                g.AddEdge(i, (i-j) % n) |> ignore
        for u, v in g.Edges do
            if random.NextDouble() < p then
                //we need to choose a new node w that is not u or v
                let choices = nodes |> Array.filter (fun n -> n <> u && n <> v)
                let w = Array.sampleOne choices 
                g.RemoveEdge(u, v) |> ignore
                g.AddEdge(u, w) |> ignore
                ()
        g

    let copyTo (source:IGraph<_>) (target:IGraph<_>) =
        let vs = source.Vertices
        for a in vs do 
            for b in vs do
                match source.ContainsEdge(a,b) with
                | Some true -> 
                    target.AddNode a; target.AddNode b
                    target.AddEdge(a,b) 
                | _ -> ()

    let getMatchingVertices f (g:IGraph<_>) = 
        [|for v in g.Vertices do if f v then yield v|]

    let copyFromEdgeList (es : _ seq) (g:IGraph<_>) =
        for (a,b) as e in es do 
            g.AddNode a
            g.AddNode b
            g.AddEdge(e) 

    let getAncestors n (g: IGraph<_>) =
        let rec loop n = 
            let parents = List.ofArray (g.Ins n)
            match parents with
            | [] -> []
            | _ -> 
                (List.collect loop parents) @ parents 
            
        loop n |> List.distinct

    //the first part of parsing the graph is to get the nodes

    let private getNodeName (nodenames: IDict<string, string>) node =
        if nodenames.ContainsKey node then
            nodenames.[node]
        else
            node

    let private getRawNodeParts (node: string) =
        let parts = node.Splitby("[")

        if parts.Length > 1 then
            parts.[0].Trim()
        else
            parts.[0].Trim()

    let private mermaidTxtLines (mgraphtxt: string) =
        mgraphtxt.Trim().Splitby("\n")
        |> Array.filter (String.contains "flowchart LR" >> not)

    let private getNodesFromMermaidTxtLines  (lines: string []) =
        let rawnodes =
            List.distinct [ for line in lines do
                                yield! line.Splitby("-->") |> Array.map String.trim ]

        //now we want to use the actual names of the nodes, but first lets make a dictionary from the raw names to the actual names
        let nodenames =
            [ for node in rawnodes do
                  let parts = node.Splitby("[")

                  if parts.Length > 1 then
                      yield parts.[0].Trim(), parts.[1].Replace("]", "").Trim() ]
            |> dict

        let nodes =
            [ for node in rawnodes do
                  if node.Length > 0 then
                      //get the rawname
                      let parts = node.Splitby("[")
                      let rawname = parts.[0].Trim()
                      getNodeName nodenames rawname ]
            |> List.distinct

        nodes, nodenames

    
    let filterNodes nodefilter (g:IGraph<_>) =
        for v in g.Vertices do
            if not (nodefilter v) then
                g.RemoveNode(v) |> ignore
        g

    let parseMermaidFlowChart (s: string) =
        let lines = mermaidTxtLines s
        let gr = DirectedGraph<string>()
        let nodes, nodenames = getNodesFromMermaidTxtLines lines

        for node in nodes do
            gr.InsertNode(node) |> ignore

        //now we need to add the edges
        for line in lines do
            let parts = line.Splitby("-->")

            if parts.Length > 1 then
                let from = getNodeName nodenames (getRawNodeParts parts.[0])
                let toNode = getNodeName nodenames (getRawNodeParts parts.[1])
                gr.InsertEdge(from, toNode) |> ignore

        gr

    //parsing flowcharts with edge labels

    let parseLabeledMermaidFlowChart (s: string) =
        let lines = mermaidTxtLines s
        let gr = GeneralDirectedGraph<string, string>()
        let nodes, nodenames = getNodesFromMermaidTxtLines lines

        for node in nodes do
            gr.InsertNode(node) |> ignore
        //parsing edges with labels. Labels are text between arrows
        for line in lines do
            //there can be text between arrow parts such as -- text -->
            // "A -- text --> B".SplitBy("--") = ["A ", " text ", "> B"]
            // "A --> B".SplitBy("--") = ["A ", "> B"]
            let parts = line.Splitby("--")

            if parts.Length > 1 then
                let from = getNodeName nodenames (getRawNodeParts parts.[0])

                if parts.Length > 2 then
                    let toNode = getNodeName nodenames (getRawNodeParts (parts.[2].Replace(">", "")))
                    let label = parts.[1].Trim()
                    gr.InsertEdge(from, toNode, label) |> ignore
                else
                    let toNode = getNodeName nodenames (getRawNodeParts (parts.[1].Replace(">", "")))
                    gr.InsertEdge(from, toNode, "") |> ignore 
        gr


type GraphAlgorithms() = 
    static member isCyclic (g:IGraph<_>) =
        let tempmarks = Hashset()
        let visited = Hashset()
        let mutable errorstate = None

        let rec hasCycles n =
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
                        |> Option.map (Array.exists hasCycles)
                        |> Option.defaultValue false
                    tempmarks.Remove n |> ignore 
                    res
        if g.IsDirected then
            if Array.exists hasCycles g.Vertices then
                Error (IsCyclic errorstate.Value)
            else Ok NotCyclic
        else Error NotaDAG

    static member private removeCyclesAux insertEdge remove store reAddEdges
                  (g : IGraph<_>) =
        let cycled = System.Collections.Generic.Stack()
        let removeds = Hashset()

        let rec reAdd l =
            match l with
            | [] -> ()
            | e :: es ->
                insertEdge e
                match GraphAlgorithms.isCyclic g with
                | Ok NotCyclic -> reAdd es
                | _ -> remove e; reAdd es

        let rec innerLoop options focusnode =
            match options with
            | (n0, _) :: ns ->
                if reAddEdges then store removeds (n0, focusnode)
                g.RemoveEdge(n0, focusnode)
                match GraphAlgorithms.isCyclic g with
                | Ok NotCyclic as ok -> ok
                | Error(IsCyclic n) as err when n <> focusnode -> err
                | Error(IsCyclic _) -> innerLoop ns focusnode
                | _ -> failwith "Unexpected error trying to remove cycles"
            | [] -> Error NotaDAG

        let rec removeCycle n =
            cycled.Push n
            let options =
                [ for v in g.Ins n ->
                      v, g.GetNodeNeighborCount v |> Option.defaultValue 0 ]
                |> List.sortByDescending snd
            match innerLoop options n with
            | Ok NotCyclic ->
                let remlist = List.ofSeq removeds
                if reAddEdges then reAdd (List.shuffle remlist)
                Ok(Seq.toArray cycled, remlist)
            | Error(IsCyclic n) -> removeCycle n
            | Error NotaDAG -> Error "Retry with node removal"

        match (GraphAlgorithms.isCyclic g) with
        | Error(IsCyclic n) -> removeCycle n
        | Ok NotCyclic -> Ok(Seq.toArray cycled, List.ofSeq removeds)
        | Error NotaDAG -> Error "Not a DAG"

    static member removeCycles (g : IGraph<_>, ?reAddEdges) =
        GraphAlgorithms.removeCyclesAux g.AddEdge g.RemoveEdge
            (fun h e -> h.Add e |> ignore) (defaultArg reAddEdges true) g

    static member removeCycles (g : IWeightedGraph<_, _>, ?reAddEdges) =
        let store (removeds : Hashset<_>) (u, v) =
            let w = g.GetEdgeValue(u, v) |> Option.get
            removeds.Add(u, v, w) |> ignore
        GraphAlgorithms.removeCyclesAux g.InsertWeightedEdge
            (fun (u, v, _) -> g.RemoveEdge(u, v)) store
            (defaultArg reAddEdges true) g

    static member private topologicalSortAux isdirected (vertices:_[]) f getEdges = 
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
        if not isdirected then Error NotaDAG
        else 
            let len = vertices.Length
            let mutable i = 0
            while i < len && not error do 
                let v = vertices.[i]
                i <- i + 1
                if not(completed.Contains v || error) then visit v 
            if error then Error (IsCyclic errorSource.Value) 
            else Result.Ok (Seq.toArray stack)

    static member private transformVertices prioritizeVertices (g:IGraph<_>) =
        match prioritizeVertices with
        | None -> g.Vertices
        | Some dir -> 
            match dir with
            | Ascending ->
                g.GetNodeNeighborCounts()
                |> Array.sortBy snd
                |> Array.map fst  
            | Descending ->
                g.GetNodeNeighborCounts()
                |> Array.sortByDescending snd
                |> Array.map fst  

    static member topologicalSort (g : IWeightedGraph<_, _>, ?NodePrioritization) =
        let vertices =
            GraphAlgorithms.transformVertices NodePrioritization g
        GraphAlgorithms.topologicalSortAux g.IsDirected vertices keyValueToKey g.GetRawEdges

    static member topologicalSort (g : IGraph<_>, ?NodePrioritization) =
        let vertices =
            GraphAlgorithms.transformVertices NodePrioritization g
        GraphAlgorithms.topologicalSortAux g.IsDirected vertices id g.GetNeighbors

    static member private extractShortestPathDijkstra f ((dists: Dict<_,_>, prevs: Dict<_,_>), target) =
        recurse (fst >> Option.isNone) 
            (fun (prev,l) ->
                match prev with 
                | Some p -> prevs.GetOrDefault(p, None), f(p,dists.[p]) :: l 
                | _ -> failwith "no path") 
            (Some target,[])
            |> snd

    static member shortestPathDijkstra g (source, target) = 
        GraphAlgorithms.extractShortestPathDijkstra fst (GraphAlgorithms.dijkstrasShortestPath(g, source, target),target)
    
    static member shortestPathsDijkstra (g : IWeightedGraph<_, _>) source = 
        let paths = GraphAlgorithms.dijkstrasShortestPath (g, source)
        g.Vertices 
        |> Array.map (fun v -> GraphAlgorithms.extractShortestPathDijkstra id (paths, v))
        
    static member dijkstrasShortestPath(g : IWeightedGraph<_, _>, source, ?target) =
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
                let adjs = g.GetRawEdges next
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
                        p.ExpandElseAdd(v, (fun _ -> u), u)
        d, p

    static member readOffPath target (prevs:Dict<_,_>) = 
        let rec loop p target =
            match prevs.TryFind target with 
            | None -> p
            | Some v -> loop (v::p) v
        loop [target] target
     
    static member shortestPath (g:IWeightedGraph<'a,_>,order:'a[],source:'a) = 
        GraphAlgorithms.extremePath (>) Double.MaxValue order g source

    static member shortestPath (g:IWeightedGraph<'a,_>, order:'a[], source:'a, target :'a) = 
        GraphAlgorithms.shortestPath(g, order, source)
        |> snd |> GraphAlgorithms.readOffPath target

    static member longestPath (g:IWeightedGraph<'a,_>, order:'a[], source:'a, target :'a) = 
        GraphAlgorithms.longestPath(g, order, source)
        |> snd |> GraphAlgorithms.readOffPath target

    static member longestPath (g:IWeightedGraph<'a,_>, order:'a[],source:'a) = 
        GraphAlgorithms.extremePath (<) Double.MinValue order g source

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
    //get current assembly, load dagre-template text file 
    let assembly = System.Reflection.Assembly.GetExecutingAssembly()
    let template = assembly.GetManifestResourceStream("Prelude.dagre-template.txt")
    //read stream bytes
    let buffer = Array.create (int template.Length) 0uy
    let templateBytes = template.Read(buffer, 0, buffer.Length)
    closeAndDispose template
    //convert bytes to string
    let dagreTemplate = System.Text.Encoding.UTF8.GetString(buffer, 0, templateBytes)

    let disp isleftright svgid w h (vs,es) =
        let rankdir = if isleftright then """rankdir: "LR",""" else ""
        dagreTemplate
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
       
    let createDagreWeightedGraph flexwidth edgeStr nodestr fixlen maxw h htmlLabels
        (g : IWeightedGraph<_,_>) = 
        let arrowType = if g.IsDirected then "" else "arrowhead: 'undirected', " 
        let labelstyle = if htmlLabels then """labelType: "html", """ else ""
        let vs =
            g.Vertices
            |> Seq.map (fun v ->
                    let maxw = match flexwidth with | Some f -> f v | None -> maxw
                    $"g.setNode('{v}', {{{labelstyle}label:'{(fixlen (nodestr v))}', width:{maxw}, height:{h}}});")
            |> String.joinWith "\n"

        let es =
            g.WeightedEdges 
            |> Array.mapi (fun i ((e1, e2), w) ->
                $"g.setEdge('{e1}', '{e2}', {{{labelstyle}{arrowType}label: '{edgeStr w}'}}, 'e{i}')") 
            |> String.joinWith "\n"

        vs, es    

    let createDagreGraph flexwidth nodestr fixlen maxw h htmlLabels
        (g : IGraph<_>) = 
        let arrowType =
            if g.IsDirected then ""
            else "arrowhead: 'undirected'"
            
        let labelstyle = if htmlLabels then """labelType: "html", """ else "" 
        
        let startBrace, endBrace = 
            if arrowType = "" && labelstyle = "" then "", "" else ", { ", " }"
        
        let vs =
            g.Vertices
            |> Seq.map (fun v ->
                let maxw = match flexwidth with | Some f -> f v | None -> maxw
                $"g.setNode('{v}', {{{labelstyle}label:'{(fixlen (nodestr v))}', width:{maxw}, height:{h}}});")
            |> String.joinWith "\n"

        let es =
            g.Edges  
            |> Array.map (fun (e1, e2) ->
                $"g.setEdge('{e1}', '{e2}'{startBrace}{labelstyle}{arrowType}{endBrace})") 
            |> String.joinWith "\n"
        vs, es     
 

    type DagreRenderer() =
        static member DrawWeightedGraph(g:IWeightedGraph<_,_>, svgid, ?maxwidth, ?height, ?canvaswidth, ?canvasheight, ?isleftright, ?maxstrlen, ?weighttostr, ?nodetostr, ?lenfix, ?flexwidth, ?isHtmlLabel) =
            let maxw, h = defaultArg maxwidth 30, defaultArg height 30
            let leftright = defaultArg isleftright false
            let nodestr = defaultArg nodetostr string
            let weightstr = defaultArg weighttostr string
            let strlen = defaultArg maxstrlen 19
            let fix = defaultArg lenfix (fixlen strlen)  
            let ishtml = defaultArg isHtmlLabel false
            let canvasw, canvash = defaultArg canvaswidth 800, defaultArg canvasheight 500 

            createDagreWeightedGraph flexwidth weightstr nodestr fix maxw h ishtml g
            |> disp leftright svgid canvasw canvash  

        static member DrawGraph(g:IGraph<_>, svgid, ?maxwidth, ?height, ?canvaswidth, ?canvasheight, ?isleftright, ?maxstrlen, ?nodetostr, ?lenfix, ?flexwidth, ?isHtmlLabel) =
            let maxw, h = defaultArg maxwidth 30, defaultArg height 30
            let leftright = defaultArg isleftright false
            let nodestr = defaultArg nodetostr string 
            let strlen = defaultArg maxstrlen 19
            let fix = defaultArg lenfix (fixlen strlen)  
            let ishtml = defaultArg isHtmlLabel false
            let canvasw, canvash = defaultArg canvaswidth 800, defaultArg canvasheight 500 

            createDagreGraph flexwidth nodestr fix maxw h ishtml g
            |> disp leftright svgid canvasw canvash  
            

    
    //====================================

    // type ForceGraphNodeAndName = {
    //     id: string;
    //     name: string;
    //     ``val`` : int
    // }

    // type ForceGraphNode = {
    //     id: string; 
    //     ``val`` : int
    // } 

    // type ForceGraphEdge = {
    //     source: string;
    //     target: string; 
    // }

    // type ForceGraph =
    //     { nodes: ForceGraphNode seq
    //       links: ForceGraphEdge seq }
    //     static member FromGraph(g: IGraph<_>) =
    //         { nodes =
    //               g.Vertices
    //               |> Seq.map (fun v ->
    //                   { id = v; ``val`` = 1 })
    //           links =
    //               g.Edges
    //               |> Array.map (fun (n1, n2) ->
    //                   { source = n1; target = n2 }) }     