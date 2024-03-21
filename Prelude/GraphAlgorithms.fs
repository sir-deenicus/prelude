namespace Prelude

open SimpleGraphs
open Common
open System
open SimpleDirectedGraphs
open Math
open Prelude.Collections.FibonacciHeap

type TopoSortError<'a> = NotDAG | Cyclic of 'a | UnExpectedError  

type SortDirection = Ascending | Descending 
   
module WeightedGraph =
    let getWeightsDict f (g: IWeightedGraph<_, _>) =
        match g.RawEdgeWeightData() with 
        | Choice1Of2 nodeEdgeWeights -> 
            let d = Dict<_,_>()
            for KeyValue(n, es) in nodeEdgeWeights do
                d.Add
                    (n, [| for KeyValue(n2, w) in es -> n2, f w |]
                         |> Dict.ofSeq)
            Choice1Of2 d
        | Choice2Of2 edgeWeights -> 
            let d = Dict<_,_>()
            for KeyValue((n1, n2), w) in edgeWeights do     
                d.Add(struct (n1, n2), f w)
            Choice2Of2 d 

    let getWeightsFilteredDict nodefilter edgefilter (g: IWeightedGraph<_, _>) =  
        match g.RawEdgeWeightData() with
        | Choice1Of2 nodeEdgeWeights -> 
            let d = Dict<_,_>()
            for KeyValue(n, es) in nodeEdgeWeights do
                if nodefilter n then
                    d.Add
                        (n,
                         [| for KeyValue(n2, w) in es do
                             if edgefilter ((n, n2), w) then yield (n2, w) |]
                         |> Dict.ofSeq)
            Choice1Of2 d
        | Choice2Of2 edgeWeights ->
            let d = Dict<_,_>()
            for KeyValue((n1, n2), w) in edgeWeights do
                if nodefilter n1 && edgefilter ((n1, n2), w) then
                    d.Add(struct (n1, n2), w)
            Choice2Of2 d 

    let edgeWeightsOfEdgesDict (d:Dict<_,_>) =
        let ew = Dict()
        for KeyValue(v1, vs) in d do  
            for KeyValue(v2, w) in vs do
                ew.Add(struct(v1, v2), w) |> ignore
        ew

    let mapWeights f (g: IWeightedGraph<_, _>) =
        let d = getWeightsDict f g
        let wg = WeightedGraph()
        match d with
        | Choice1Of2 nodeEdgeWeights -> 
            wg.FromDictionary (edgeWeightsOfEdgesDict nodeEdgeWeights)
        | Choice2Of2 edgeWeights ->
            wg.FromDictionary edgeWeights
        wg

    let filter nodefilter edgefilter (g: IWeightedGraph<_, _>) = 
        let d = getWeightsFilteredDict nodefilter edgefilter g 
        let wg = WeightedGraph()
        match d with
        | Choice1Of2 nodeEdgeWeights -> 
            wg.FromDictionary (edgeWeightsOfEdgesDict nodeEdgeWeights)
        | Choice2Of2 edgeWeights ->
            wg.FromDictionary edgeWeights 
        wg  
    
    ///for example, given a list [a,b,c] if these are connected in the source graph, they will be connected in the target graph
    ///which should ideally be empty.
    let ofNodeList (source:IWeightedGraph<_,_>, target:IWeightedGraph<_,_>) (nodes : _ seq) =
        for a in nodes do 
            for b in nodes do
                match source.GetEdgeValue(a,b) with 
                | None -> ()
                | Some w -> 
                    target.AddNode a; target.AddNode b
                    target.AddWeightedEdge(a,b,w) 

    let ofEdgeList (g:IWeightedGraph<_,_>) (es : _ seq) =
        for (a,b) in es do 
            match g.GetEdgeValue(a,b) with
            | None -> () 
            | Some w -> 
                g.AddNode a; g.AddNode b
                g.AddWeightedEdge(a,b,w) 

    let ofWeightedEdgeList (g:IWeightedGraph<_,_>) (es : _ seq) =
        for e in es do  
            g.AddWeightedEdge e

module WeightedDirectedGraph =
    let mapWeights f (g: IWeightedGraph<_, _>) =
        let d = WeightedGraph.getWeightsDict f g
        
        let wg = WeightedDirectedGraph()
        match d with
        | Choice1Of2 nodeEdgeWeights -> 
            wg.FromDictionary (nodeEdgeWeights)
        | Choice2Of2 edgeWeights ->
            wg.FromDictionary edgeWeights
        wg 

    let filter nodefilter edgefilter (g: IWeightedGraph<_, _>) = 
        let d = WeightedGraph.getWeightsFilteredDict nodefilter edgefilter g 
        let wg = WeightedDirectedGraph()
        match d with
        | Choice1Of2 nodeEdgeWeights -> 
            wg.FromDictionary (nodeEdgeWeights)
        | Choice2Of2 edgeWeights ->
            wg.FromDictionary edgeWeights
        wg   

module DirectedMultiGraph =     
    let inline mergeNodes reduceNodeWeights (g:DirectedMultiGraph<_, _>) =
        Array.iter (g.ModifyNodeEdges reduceNodeWeights) g.Nodes

    let mapWeights f (g: DirectedMultiGraph<_, _>) =
        let d = Dict<_, _>()

        for KeyValue(n, es) in g.EdgeData do
            d.Add(n, [| for KeyValue(n2, ws) in es -> n2, ResizeArray(Seq.map f ws) |] |> Dict.ofSeq)

        let wg = DirectedMultiGraph()
        wg.FromDictionary d
        wg 
        
    let getWeightsFilteredDict nodefilter edgefilter (g: DirectedMultiGraph<_, _>) = 
        let d = Dict<_,_>()
        for (KeyValue(sourceNode, edges)) in g.EdgeData do
            if nodefilter sourceNode then
                d.Add
                    (sourceNode,
                     [| for KeyValue(targetNode, w) in edges do
                         if edgefilter ((sourceNode, targetNode), w) then yield (targetNode, w) |]
                     |> Dict.ofSeq)
        d

    let filter nodefilter edgefilter (g: DirectedMultiGraph<_, _>) = 
        let d = getWeightsFilteredDict nodefilter edgefilter g 
        let wg = DirectedMultiGraph()
        wg.FromDictionary d
        wg  

    let filterNodes nodefilter (g: DirectedMultiGraph<_, _>) = 
        for v in g.Nodes do
            if not (nodefilter v) then
                g.Remove(v) |> ignore
        g  
 
            
module Graph = 
    //implement BFS 
    let breadthFirstSearch (g: IGraph<_>) stopcondition startNode =
        let visited = Hashset()
        let queue = Collections.Generic.Queue()

        visited.Add startNode |> ignore
        queue.Enqueue startNode |> ignore

        let mutable stopSearch = false
        let mutable result = None

        while queue.Count > 0 && not stopSearch do
            let node = queue.Dequeue()
            if stopcondition node then
                result <- Some node
                stopSearch <- true
            else
                for n in g.GetNeighbors node do
                    if not (visited.Contains n) then
                        visited.Add n |> ignore
                        queue.Enqueue n  
        result
         

    let internal sortNodesByDegree prioritizeNodes (g:IGraph<_>) =
        match prioritizeNodes with
        | None -> g.Nodes
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

    let randomTopLevelNode (g: IGraph<_>) =
        if g.IsDirected then
            let toplevelcandidates = [| for n in g.Nodes do if Array.isEmpty (g.Ins n) then yield n |]
            if Array.isEmpty toplevelcandidates then
                sortNodesByDegree (Some Descending) g |> Array.head
            else
                Array.sampleOne toplevelcandidates
        else sortNodesByDegree (Some Descending) g |> Array.head
            
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
        let vs = source.Nodes
        for a in vs do 
            for b in vs do
                match source.ContainsEdge(a,b) with
                | Some true -> 
                    target.AddNode a; target.AddNode b
                    target.AddEdge(a,b) 
                | _ -> ()

    let getMatchingVertices f (g:IGraph<_>) = 
        [|for v in g.Nodes do if f v then yield v|]

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
        for v in g.Nodes do
            if not (nodefilter v) then
                g.RemoveNode(v) |> ignore
        g

    let parseMermaidFlowChart (s: string) =
        let lines = mermaidTxtLines s
        let gr = DirectedGraph<string>()
        let nodes, nodenames = getNodesFromMermaidTxtLines lines

        for node in nodes do
            gr.AddNode(node) |> ignore

        //now we need to add the edges
        for line in lines do
            let parts = line.Splitby("-->")

            if parts.Length > 1 then
                let from = getNodeName nodenames (getRawNodeParts parts.[0])
                let toNode = getNodeName nodenames (getRawNodeParts parts.[1])
                gr.AddEdge(from, toNode) |> ignore

        gr

    //parsing flowcharts with edge labels

    let parseLabeledMermaidFlowChart (s: string) =
        let lines = mermaidTxtLines s
        let gr = GeneralDirectedGraph<string, string>()
        let nodes, nodenames = getNodesFromMermaidTxtLines lines

        for node in nodes do
            gr.AddNode(node) |> ignore
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
                    gr.AddEdge(from, toNode, label) |> ignore
                else
                    let toNode = getNodeName nodenames (getRawNodeParts (parts.[1].Replace(">", "")))
                    gr.AddEdge(from, toNode, "") |> ignore 
        gr


type GraphAlgorithms() =  

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
                | NotCyclic -> reAdd es
                | _ -> remove e; reAdd es

        let rec innerLoop options focusnode =
            match options with
            | (currentNode, _) :: ns ->
                if reAddEdges then store removeds (currentNode, focusnode)
                g.RemoveEdge(currentNode, focusnode)
                match GraphAlgorithms.isCyclic g with
                | NotCyclic as notCyclicResult -> Some notCyclicResult
                | IsCyclic n as cyclicNode when n <> focusnode -> Some cyclicNode
                | IsCyclic _ -> innerLoop ns focusnode                 
            | [] -> None

        let rec removeCycle n =
            cycled.Push n
            let options =
                let adjacentNodes = if g.IsDirected then g.Ins n else g.GetNeighbors n
                [ for v in adjacentNodes do
                      v, g.GetNodeNeighborCount v |> Option.defaultValue 0 ]
                |> List.sortByDescending snd
            match innerLoop options n with
            | Some NotCyclic ->
                let remlist = List.ofSeq removeds
                if reAddEdges then reAdd (List.shuffle remlist)
                Ok(Seq.toArray cycled, remlist)
            | Some (IsCyclic n) -> removeCycle n 
            | None -> Error UnExpectedError

        match (GraphAlgorithms.isCyclic g) with
        | IsCyclic n -> removeCycle n
        | NotCyclic -> Ok(Seq.toArray cycled, List.ofSeq removeds) 

    static member removeCycles (g : IGraph<_>, ?reAddEdges) =
        GraphAlgorithms.removeCyclesAux g.AddEdge g.RemoveEdge
            (fun h e -> h.Add e |> ignore) (defaultArg reAddEdges true) g

    static member removeCycles (g : IWeightedGraph<_, _>, ?reAddEdges) =
        let store (removeds : Hashset<_>) (u, v) =
            let w = g.GetEdgeValue(u, v) |> Option.get
            removeds.Add(u, v, w) |> ignore
        GraphAlgorithms.removeCyclesAux g.AddWeightedEdge
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
                    for m in getEdges n do
                        if not error then visit (f m) 
                    tempmarks.Remove n |> ignore
                    completed.Add n |> ignore 
                    stack.Push n    
        if not isdirected then Error NotDAG
        else 
            let len = vertices.Length
            let mutable i = 0
            while i < len && not error do 
                let v = vertices.[i]
                i <- i + 1
                if not(completed.Contains v || error) then visit v 
            if error then Error (Cyclic errorSource.Value) 
            else Result.Ok (Seq.toArray stack) 

    static member topologicalSort (g : IWeightedGraph<_, _>, ?NodeSortingPrioritization) =
        let vertices =
            Graph.sortNodesByDegree NodeSortingPrioritization g
        GraphAlgorithms.topologicalSortAux g.IsDirected vertices fst g.GetWeightedEdges

    static member topologicalSort (g : IGraph<_>, ?NodeSortingPrioritization) =
        let vertices =
            Graph.sortNodesByDegree NodeSortingPrioritization g
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
        g.Nodes 
        |> Array.map (fun v -> GraphAlgorithms.extractShortestPathDijkstra id (paths, v))
        
    static member private shortOrLongestPath comparer seedvalue (order:'a[]) (g:IWeightedGraph<'a,_>) (s:'a) =
        if not g.IsDirected || (Option.defaultValue true g.HasCycles) then failwith "Not a DAG"
        let subpath = Array.skipWhile ((<>) s) order
        let d = Array.map (fun n -> n, seedvalue) subpath |> Dict.ofSeq
        d[s] <- 0.
        let p = Dict()
        for u in subpath do
            let vs = g.GetWeightedEdges u  
            for (v,w) in vs do 
                if comparer d[v] (d[u] + w) then
                    d[v] <- d[u] + w
                    p.ExpandElseAdd(v, (fun _ -> u), u)
        d, p

    static member readOffPath target (prevs:Dict<_,_>) = 
        let rec loop p target =
            match prevs.TryFind target with 
            | None -> p
            | Some v -> loop (v::p) v
        loop [target] target

    static member shortestPathDAG (g:IWeightedGraph<'a,_>,order:'a[],source:'a) =
        GraphAlgorithms.shortOrLongestPath (>) Double.PositiveInfinity order g source
    
    static member longestPathDAG (g:IWeightedGraph<'a,_>,order:'a[],source:'a) =
        GraphAlgorithms.shortOrLongestPath (<) Double.NegativeInfinity order g source

    static member shortestPathDAG (g:IWeightedGraph<'a,_>, order:'a[], source:'a, target :'a) = 
        GraphAlgorithms.shortestPathDAG(g, order, source)
        |> snd 
        |> GraphAlgorithms.readOffPath target

    static member longestPathDAG (g:IWeightedGraph<'a,_>, order:'a[], source:'a, target :'a) =
        GraphAlgorithms.longestPathDAG(g, order, source)
        |> snd 
        |> GraphAlgorithms.readOffPath target 

    static member dijkstrasShortestPath(g : IWeightedGraph<_, _>, source, ?target) =
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
                let vs = g.GetWeightedEdges next
                let _ = visited.Add next 
                for (node2,weight) in vs do
                    if not (visited.Contains node2) then
                        let newDistance = dists[next] + weight
                        if newDistance < dists[node2] then
                            dists[node2] <- newDistance
                            prev[node2] <- Some next
                            FibHeap.decrease_key q nodeMap.[node2] newDistance
                false) (false)
        |> ignore
        dists, prev

    static member minimumSpanningTree(g : IWeightedGraph<_, _>, ?domax, ?NodePrioritization) =
        let dir =
            if (defaultArg domax false) then -1.
            else 1.

        let currentCut = 
            Hashset(Graph.sortNodesByDegree NodePrioritization g)
        let root = currentCut |> Seq.head
        let fs = Collections.Generic.SortedSet()
        let buildTree (tree:IWeightedGraph<_,_>) =
            let _ =
                recurse (fun _ -> currentCut.Count = 0) (fun (node1, i, getnodes) ->
                    if getnodes then
                        for (node2, weight) in g.GetWeightedEdges node1 do
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
                            tree.AddWeightedEdge(v1, v2, w)
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
          
    /// <summary>
    /// Retrieves the neighbors of a given node in a graph, with additional options to specify the degree of separation and a filter function.
    /// </summary>
    /// <param name="g">The graph from which to retrieve the neighbors.</param>
    /// <param name="node">The node for which to retrieve the neighbors.</param>
    /// <param name="Degree">Optional parameter. Specifies the degree of separation between the given node and its neighbors. A degree of 1 retrieves immediate neighbors, a degree of 2 retrieves neighbors of neighbors, and so on. Defaults to 1 if not specified.</param>
    /// <param name="Filter">Optional parameter. A function that filters the neighbors based on a condition. The function takes a node as input and returns a boolean. If the function returns true, the node is included in the result; if it returns false, the node is excluded. By default, all nodes are included.</param>
    /// <returns>A list of tuples, where each tuple contains an array of nodes and an integer. The array of nodes represents the neighbors at a certain degree of separation, and the integer represents the degree of separation.</returns>
    static member getNeighbors(g:IGraph<_>, node, ?Degree, ?Filter) = 
        let filter = defaultArg Filter (fun _ -> true)
        let degree = defaultArg Degree 1
    
        let rec getAllNodes deg n =
            if deg = 0 then []
            else
                let nodes =
                    g.GetNeighbors n 
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
            g.Nodes
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
            g.Nodes
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