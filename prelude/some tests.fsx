#time "on"
#nowarn "1125"
 
#I @"..\..\DictionarySlim\bin\Release\netstandard2.1"
#r "DictionarySlim.dll"
#r @"bin\Debug\net5\Prelude.dll"

open Prelude.Math
open System
open Prelude.Common
open Prelude.Parallel
open Prelude.TrieDictionarySearch 
open System.Net
open Prelude.SimpleGraphs
open Prelude.StringMetrics

open Prelude.SimpleTrees
open Prelude.SimpleDirectedGraphs
open Prelude 

let g1 = DirectedGraph<int>()

for i in 0..5 do g1.AddNode i |> ignore

g1.AddEdge(5,2) |> ignore
g1.AddEdge(5,0) |> ignore
g1.AddEdge(4,0) |> ignore
g1.AddEdge(4,1) |> ignore
g1.AddEdge(2,3) |> ignore
g1.AddEdge(3,1) |> ignore

let g2 = DirectedGraph<char>()

for c in 'A'..'F' do g2.AddNode c |> ignore

g2.AddEdge('A','B') |> ignore
g2.AddEdge('A','D') |> ignore
g2.AddEdge('B','C') |> ignore
g2.AddEdge('C','D') |> ignore
g2.AddEdge('C','E') |> ignore
g2.AddEdge('D','E') |> ignore
g2

let g3 = DirectedGraph<int>()

for v in [5;11;2;7;8;9;3;10] do g3.AddNode v |> ignore

g3.AddEdge(5,11) |> ignore
g3.AddEdge(11,2) |> ignore
g3.AddEdge(7,11) |> ignore
g3.AddEdge(7,8) |> ignore
g3.AddEdge(8,9) |> ignore
g3.AddEdge(3,8) |> ignore
g3.AddEdge(3,10) |> ignore
g3.AddEdge(11,9) |> ignore
g3.AddEdge(11,10) |> ignore


let gi = CompressedDirectedGraph<int,float, _>(byte) 
for i in 1..15 do
    gi.AddNode i |> ignore
    
for x in 2..15 do
    for y in 2..15 do 
        if x <> y && x % y = 0 then
            gi.AddEdge(x,y,1.) |> ignore

gi.ComputeReverseIndex()    

let kv = Dict.ofIDict (dict ["A", ResizeArray [1..10]])

for KeyValue(k,sq) in kv do 
    for i in 0..sq.Count - 1 do sq[i] <- sq[i] * 2

kv
//get current assembly, load dagre-template text file 
let assembly = typeof<DirectedGraph<int>>.Assembly

let template = assembly.GetManifestResourceStream("Prelude.dagre-template.txt")
//read stream bytes
let buffer = Array.create (int template.Length) 0uy
let templateBytes = template.Read(buffer, 0, buffer.Length)
closeAndDispose template
//convert bytes to string
System.Text.Encoding.UTF8.GetString(buffer, 0, templateBytes)

let page = IO.File.ReadAllText @"C:\Users\cybernetic\Documents\Papers\dagre-template.txt" 
let disptemplate = IO.File.ReadAllText @"C:\Users\cybernetic\Documents\Papers\disp-template.txt"

let disp2(gd:IGraph<string>) =
    let gtxt = GraphVisualization.createDagreGraph None string id 30 30 false gd
    let fout = disptemplate.Replace ("__TEXT__", GraphVisualization.disp true "n1" 1200 1200 gtxt)
    IO.File.WriteAllText(@"C:\Users\cybernetic\Documents\Papers\disp.htm", fout)   

let disp(g0:IWeightedGraph<string,float>) =
    let gtxt = GraphVisualization.createDagreWeightedGraph None string string id 30 30 false g0
    let fout = disptemplate.Replace ("__TEXT__", GraphVisualization.disp false "n1" 800 500 gtxt)
    IO.File.WriteAllText(@"C:\Users\cybernetic\Documents\Papers\disp.htm", fout)
    

     
let gd = DirectedGraph<string>()
 
gd.AddNode "X" |> ignore; gd.AddNode "Y" |> ignore; gd.AddNode "Z" |> ignore; gd.AddNode "U" |> ignore
gd.AddNode "A" |> ignore
gd.AddNode "B" |> ignore
gd.AddNode "C" |> ignore
gd.AddNode "D" |> ignore
gd.AddNode "E" |> ignore
gd.AddNode "F" |> ignore
gd.AddNode "G" |> ignore
gd.AddEdge("U", "X") |> ignore
gd.AddEdge("U", "Y") |> ignore
gd.AddEdge("X", "Y") |> ignore
gd.AddEdge("Z", "X") |> ignore
gd.AddEdge("B", "A") |> ignore
gd.AddEdge("C", "B") |> ignore
gd.AddEdge("A", "C") |> ignore
gd.AddEdge("D", "B" ) |> ignore
gd.AddEdge("D", "C" ) |> ignore
gd.AddEdge("E", "D" ) |> ignore
gd.AddEdge("C", "E") |> ignore
gd.AddEdge("F", "D") |> ignore
gd.AddEdge("E", "F") |> ignore
gd.AddEdge("A", "F") |> ignore
gd.AddEdge("F", "G" ) |> ignore
gd.AddEdge("E", "G" ) |> ignore
gd.AddEdge("D", "G" ) |> ignore
gd.AddEdge("G", "D" ) |> ignore
gd.AddEdge("F", "F") |> ignore
gd.AddEdge("G", "C" ) |> ignore
gd.AddEdge("G", "Y" ) |> ignore
gd.AddEdge("B", "G" ) |> ignore
gd.AddEdge("A", "Z" ) |> ignore
gd.AddEdge("Y", "D" ) |> ignore
 
GraphAlgorithms.removeCycles gd
  

                
Branch("B", [Node "A"; Branch("C", [Node "D"; Branch("E", [Node "E1"; Node "E1b"])]); Node "F"])
|> dispTree id

graphToTree gd "X" |> dispTree id  

disp2 gd

let disconnectedSubGraphs (all:Hashset<_>) (first, g:IGraph<_>) = 
    let seen = Hashset()
    let edges = Hashset()
    let rec build v =
        let inNodes = if g.IsDirected then g.Ins v else Array.empty 
        let outNodes = g.GetNeighbors v
        seen.Add v |> ignore; all.Remove v |> ignore
        for v2 in inNodes do  
            edges.Add(v2, v) |> ignore
            if not(seen.Contains v2) then build v2
        for v2 in outNodes do 
            edges.Add(v,v2) |> ignore
            if not(seen.Contains v2) then build v2
    build first
    Seq.toArray edges

let ffb (all:Hashset<_>) (first, g:IGraph<_>) = 
    let seen = Hashset()
    let rec build v =
        [|  let inNodes = if g.IsDirected then g.Ins v else Array.empty
                          |> Array.filter (seen.Contains >> not)
            let outNodes = g.GetNeighbors v
                           |> Array.filter (seen.Contains >> not)
            seen.Add v |> ignore; all.Remove v |> ignore
            for v2 in inNodes do  
                yield (v2,v)  
                if not(seen.Contains v2) then yield! build v2
            for v2 in outNodes do 
                yield (v, v2)  
                if not(seen.Contains v2) then yield! build v2|] 
    build first 
    
let ff2 (first, g:IGraph<_>) = 
    let all = Hashset(g.Nodes)
    let rec loop node1 =
        [| yield (ffb all (node1, g)) 
           if all.Count > 0 then yield ffb all (Seq.head all, g)|] 
    defaultArg first (Seq.head all)
    |> loop 
 
let gd2 = DirectedGraph<string>()
gd2.AddNode "X" |> ignore
gd2.AddNode "Y" |> ignore
gd2.AddNode "Z" |> ignore
gd2.AddNode "U" |> ignore
gd2.AddNode "A" |> ignore
gd2.AddNode "B" |> ignore
gd2.AddNode "C" |> ignore
gd2.AddNode "D" |> ignore
gd2.AddEdge("U", "X") |> ignore
gd2.AddEdge("U", "Y") |> ignore
gd2.AddEdge("X", "Y") |> ignore
gd2.AddEdge("Z", "X") |> ignore
gd2.AddEdge("B", "A") |> ignore
gd2.AddEdge("C", "B") |> ignore
gd2.AddEdge("A", "C") |> ignore
gd2.AddEdge("D", "B" ) |> ignore
 
let first = Seq.head gd2.Nodes
let all = Hashset gd2.Nodes
Seq.toArray all


for _ in 1..100000 do ffb all (first,gd2) |> ignore

disp2 gd2



let g0 = CompressedDirectedGraph<string,float,_>(uint16, true)
 
g0.AddNode "X" |> ignore; g0.AddNode "Y" |> ignore; g0.AddNode "Z" |> ignore; g0.AddNode "U" |> ignore
g0.AddEdge("U", "X", 1.) |> ignore
g0.AddEdge("U", "Y", 1.) |> ignore
g0.AddEdge("X", "Y", 1.) |> ignore
g0.AddEdge("Z", "X", 1.) |> ignore

g0.AddNode "A" |> ignore
g0.AddNode "B" |> ignore
g0.AddNode "C" |> ignore
g0.AddNode "D" |> ignore
g0.AddNode "E" |> ignore
g0.AddNode "F" |> ignore
g0.AddNode "G" |> ignore
g0.AddEdge("B", "A" , 2.) |> ignore
g0.AddEdge("C", "B" , 3.) |> ignore
g0.AddEdge("A", "C" , 3.) |> ignore
g0.AddEdge("D", "B" , 1.) |> ignore
g0.AddEdge("D", "C" , 1.) |> ignore
g0.AddEdge("E", "D" , 1.) |> ignore
g0.AddEdge("C", "E" ,5.3) |> ignore
g0.AddEdge("F", "D" ,4.) |> ignore
g0.AddEdge("E", "F" ,2.) |> ignore
g0.AddEdge("A", "F" ,6.) |> ignore
g0.AddEdge("F", "G" ,5.2) |> ignore
g0.AddEdge("E", "G" ,5.2) |> ignore
g0.AddEdge("D", "G" ,5.2) |> ignore
g0.AddEdge("G", "D" ,5.2) |> ignore
g0.AddEdge("F", "F" ,5.21) |> ignore
g0.AddEdge("G", "C" ,5.2) |> ignore
g0.AddEdge("G", "Y" ,5.2) |> ignore
g0.AddEdge("B", "G",1. ) |> ignore
g0.AddEdge("A", "Z",1. ) |> ignore
g0.AddEdge("Y", "D",1. ) |> ignore

g0.ComputeReverseIndex()  
disp g0

GraphAlgorithms.removeCycles(gd2, false)
|> ignore

disp g0
GraphAlgorithms.getNeighbors(g0, "F", 4)
|> List.groupBy snd
|> List.mapRight (List.map fst)
    
g0
g0.Edges
g0.ForEachEdge ((+) 1.)

g0.Ins "A"
g0.Ins "C"
g0
disp2 gd2
//let (Ok order) = 
GraphAlgorithms.isCyclic gd2
GraphAlgorithms.topologicalSort gd2

for _ in 1..1_000_000 do GraphAlgorithms.isCyclic g0 |> ignore
 
let tc = 
    match GraphAlgorithms.minimumSpanningTree g0 with
    | Choice1Of2 tree -> tree
    | Choice2Of2 _ -> failwith "Expected directed spanning tree"

let order =
    match GraphAlgorithms.topologicalSort gd2 with
    | Ok order -> order
    | Error err -> failwithf "%A" err

GraphAlgorithms.shortestPathDAG(gd2.ToWeightedGraph(), order, "U")
|> snd 
|> GraphAlgorithms.readOffPath "Y"   

let inline expectNear label expected actual =
    if abs (expected - actual) > 1e-9 then
        failwithf "%s expected %f but got %f" label expected actual

let expectNegativeCycle label result =
    match result with
    | Error (NegativeWeightCycle _) -> ()
    | Ok value -> failwithf "%s expected NegativeWeightCycle but got %A" label value

let expectEqual label expected actual =
    if expected <> actual then
        failwithf "%s expected %A but got %A" label expected actual

let expectTrace label expected (trace: ResizeArray<_>) =
    expectEqual label expected (List.ofSeq trace)

let negativeWeightGraph = WeightedDirectedGraph<string>()
for node in ["S"; "A"; "B"; "T"] do
    negativeWeightGraph.AddNode node |> ignore
negativeWeightGraph.AddEdge("S", "A", 2.) |> ignore
negativeWeightGraph.AddEdge("S", "B", 5.) |> ignore
negativeWeightGraph.AddEdge("A", "B", -4.) |> ignore
negativeWeightGraph.AddEdge("A", "T", 5.) |> ignore
negativeWeightGraph.AddEdge("B", "T", 2.) |> ignore

let negativeWeightPaths =
    match GraphAlgorithms.bellmanFordsShortestPath(negativeWeightGraph, "S", "T") with
    | Ok paths -> paths
    | Error err -> failwithf "Negative weight graph should not fail: %A" err

let negativeWeightDists, _ = negativeWeightPaths
expectNear "Bellman-Ford target distance with negative weights" 0. negativeWeightDists["T"]

match GraphAlgorithms.shortestPathBellmanFord(negativeWeightGraph, "S", "T") with
| Ok ["S"; "A"; "B"; "T"] -> ()
| Ok path -> failwithf "Unexpected Bellman-Ford path for negative-weight graph: %A" path
| Error err -> failwithf "Negative weight graph should not fail: %A" err

let negativeCycleGraph = WeightedDirectedGraph<string>()
for node in ["S"; "A"; "B"; "T"] do
    negativeCycleGraph.AddNode node |> ignore
negativeCycleGraph.AddEdge("S", "A", 1.) |> ignore
negativeCycleGraph.AddEdge("A", "B", 1.) |> ignore
negativeCycleGraph.AddEdge("B", "A", -3.) |> ignore
negativeCycleGraph.AddEdge("B", "T", 1.) |> ignore

GraphAlgorithms.bellmanFordsShortestPath(negativeCycleGraph, "S")
|> expectNegativeCycle "Single-source Bellman-Ford should detect reachable negative cycles"

GraphAlgorithms.bellmanFordsShortestPath(negativeCycleGraph, "S", "T")
|> expectNegativeCycle "Target Bellman-Ford should detect negative cycles that can reach the target"

GraphAlgorithms.shortestPathBellmanFord(negativeCycleGraph, "S", "T")
|> expectNegativeCycle "Shortest-path Bellman-Ford wrapper should surface target-relevant negative cycles"

let offTargetNegativeCycleGraph = WeightedDirectedGraph<string>()
for node in ["S"; "A"; "B"; "T"] do
    offTargetNegativeCycleGraph.AddNode node |> ignore
offTargetNegativeCycleGraph.AddEdge("S", "T", 2.) |> ignore
offTargetNegativeCycleGraph.AddEdge("S", "A", 1.) |> ignore
offTargetNegativeCycleGraph.AddEdge("A", "B", 1.) |> ignore
offTargetNegativeCycleGraph.AddEdge("B", "A", -3.) |> ignore

GraphAlgorithms.bellmanFordsShortestPath(offTargetNegativeCycleGraph, "S")
|> expectNegativeCycle "Single-source Bellman-Ford should still fail when a reachable negative cycle exists off target"

let offTargetPaths =
    match GraphAlgorithms.bellmanFordsShortestPath(offTargetNegativeCycleGraph, "S", "T") with
    | Ok paths -> paths
    | Error err -> failwithf "Target Bellman-Ford should ignore negative cycles that cannot reach the target: %A" err

let offTargetDists, _ = offTargetPaths
expectNear "Bellman-Ford target distance should stay finite when the negative cycle is off target" 2. offTargetDists["T"]

match GraphAlgorithms.shortestPathBellmanFord(offTargetNegativeCycleGraph, "S", "T") with
| Ok ["S"; "T"] -> ()
| Ok path -> failwithf "Unexpected Bellman-Ford path for off-target negative-cycle graph: %A" path
| Error err -> failwithf "Target Bellman-Ford should ignore negative cycles that cannot reach the target: %A" err

let undirectedMessageGraph = UndirectedGraph<int>()
undirectedMessageGraph.AddEdge(1, 2) |> ignore

let undirectedSyncTrace = ResizeArray<int * int>()
undirectedMessageGraph.SendMessageToNeighbors(0, 1, fun count (src, dst) ->
    undirectedSyncTrace.Add(src, dst)
    count + 1, true)
expectTrace "Undirected graph sync message passing should stop globally" [(1, 2)] undirectedSyncTrace

let undirectedAsyncTrace = ResizeArray<int * int>()
undirectedMessageGraph.SendMessageToNeighborsAsync(0, 1, fun count (src, dst) ->
    undirectedAsyncTrace.Add(src, dst)
    count + 1, true)
|> Async.RunSynchronously
expectTrace "Undirected graph async message passing should stop globally" [(1, 2)] undirectedAsyncTrace

let weightedUndirectedMessageGraph = WeightedGraph<int>()
weightedUndirectedMessageGraph.AddEdge(1, 2, 1.) |> ignore
weightedUndirectedMessageGraph.AddEdge(1, 3, 1.) |> ignore

let weightedUndirectedSyncTrace = ResizeArray<int * int>()
weightedUndirectedMessageGraph.SendMessageToNeighbors(0, 1, fun count (src, dst, _weight) ->
    weightedUndirectedSyncTrace.Add(src, dst)
    count + 1, true)
expectTrace "Weighted undirected graph sync message passing should stop globally" [(1, 2)] weightedUndirectedSyncTrace

let weightedUndirectedAsyncTrace = ResizeArray<int * int>()
weightedUndirectedMessageGraph.SendMessageToNeighborsAsync(0, 1, fun count (src, dst, _weight) ->
    weightedUndirectedAsyncTrace.Add(src, dst)
    count + 1, true)
|> Async.RunSynchronously
expectTrace "Weighted undirected graph async message passing should stop globally" [(1, 2)] weightedUndirectedAsyncTrace

let weightedDirectedChildrenGraph = WeightedDirectedGraph<int>()
weightedDirectedChildrenGraph.AddEdge(1, 2, 1.) |> ignore
weightedDirectedChildrenGraph.AddEdge(1, 3, 1.) |> ignore

let weightedDirectedChildrenSyncTrace = ResizeArray<int * int>()
weightedDirectedChildrenGraph.SendMessageToChildren(0, 1, fun count (src, dst, _weight) ->
    weightedDirectedChildrenSyncTrace.Add(src, dst)
    count + 1, true)
expectTrace "Weighted directed graph sync child message passing should stop globally" [(1, 2)] weightedDirectedChildrenSyncTrace

let weightedDirectedChildrenAsyncTrace = ResizeArray<int * int>()
weightedDirectedChildrenGraph.SendMessageToChildrenAsync(0, 1, fun count (src, dst, _weight) ->
    weightedDirectedChildrenAsyncTrace.Add(src, dst)
    count + 1, true)
|> Async.RunSynchronously
expectTrace "Weighted directed graph async child message passing should stop globally" [(1, 2)] weightedDirectedChildrenAsyncTrace

let weightedDirectedParentsGraph = WeightedDirectedGraph<int>()
weightedDirectedParentsGraph.AddEdge(1, 3, 1.) |> ignore
weightedDirectedParentsGraph.AddEdge(2, 3, 1.) |> ignore

let weightedDirectedParentsSyncTrace = ResizeArray<int * int>()
weightedDirectedParentsGraph.SendMessageToParents(0, 3, fun count (src, dst, _weight) ->
    weightedDirectedParentsSyncTrace.Add(src, dst)
    count + 1, true)
expectTrace "Weighted directed graph sync parent message passing should stop globally" [(3, 1)] weightedDirectedParentsSyncTrace

let weightedDirectedParentsAsyncTrace = ResizeArray<int * int>()
weightedDirectedParentsGraph.SendMessageToParentsAsync(0, 3, fun count (src, dst, _weight) ->
    weightedDirectedParentsAsyncTrace.Add(src, dst)
    count + 1, true)
|> Async.RunSynchronously
expectTrace "Weighted directed graph async parent message passing should stop globally" [(3, 1)] weightedDirectedParentsAsyncTrace

let weightedDirectedNeighborsGraph = WeightedDirectedGraph<int>()
weightedDirectedNeighborsGraph.AddEdge(1, 2, 1.) |> ignore

let weightedDirectedNeighborsSyncTrace = ResizeArray<int * int>()
weightedDirectedNeighborsGraph.SendMessageToNeighbors(0, 1, fun count (src, dst, _weight) ->
    weightedDirectedNeighborsSyncTrace.Add(src, dst)
    let nextCount = count + 1
    nextCount, nextCount >= 6)
expectTrace "Weighted directed graph sync neighbor message passing should not bounce" [(1, 2)] weightedDirectedNeighborsSyncTrace

let weightedDirectedNeighborsAsyncTrace = ResizeArray<int * int>()
weightedDirectedNeighborsGraph.SendMessageToNeighborsAsync(0, 1, fun count (src, dst, _weight) ->
    weightedDirectedNeighborsAsyncTrace.Add(src, dst)
    let nextCount = count + 1
    nextCount, nextCount >= 6)
|> Async.RunSynchronously
expectTrace "Weighted directed graph async neighbor message passing should not bounce" [(1, 2)] weightedDirectedNeighborsAsyncTrace

let generalDirectedChildrenGraph = GeneralDirectedGraph<int, float>()
generalDirectedChildrenGraph.AddEdge(1, 2, 1.) |> ignore
generalDirectedChildrenGraph.AddEdge(1, 3, 1.) |> ignore

let generalDirectedChildrenSyncTrace = ResizeArray<int * int>()
generalDirectedChildrenGraph.SendMessageToChildren(0, 1, fun count (src, dst, _weight) ->
    generalDirectedChildrenSyncTrace.Add(src, dst)
    count + 1, true)
expectTrace "General directed graph sync child message passing should stop globally" [(1, 2)] generalDirectedChildrenSyncTrace

let generalDirectedChildrenAsyncTrace = ResizeArray<int * int>()
generalDirectedChildrenGraph.SendMessageToChildrenAsync(0, 1, fun count (src, dst, _weight) ->
    generalDirectedChildrenAsyncTrace.Add(src, dst)
    count + 1, true)
|> Async.RunSynchronously
expectTrace "General directed graph async child message passing should stop globally" [(1, 2)] generalDirectedChildrenAsyncTrace

let generalDirectedParentsGraph = GeneralDirectedGraph<int, float>()
generalDirectedParentsGraph.AddEdge(1, 3, 1.) |> ignore
generalDirectedParentsGraph.AddEdge(2, 3, 1.) |> ignore

let generalDirectedParentsSyncTrace = ResizeArray<int * int>()
generalDirectedParentsGraph.SendMessageToParents(0, 3, fun count (src, dst, _weight) ->
    generalDirectedParentsSyncTrace.Add(src, dst)
    count + 1, true)
expectTrace "General directed graph sync parent message passing should stop globally" [(3, 1)] generalDirectedParentsSyncTrace

let generalDirectedParentsAsyncTrace = ResizeArray<int * int>()
generalDirectedParentsGraph.SendMessageToParentsAsync(0, 3, fun count (src, dst, _weight) ->
    generalDirectedParentsAsyncTrace.Add(src, dst)
    count + 1, true)
|> Async.RunSynchronously
expectTrace "General directed graph async parent message passing should stop globally" [(3, 1)] generalDirectedParentsAsyncTrace

let generalDirectedNeighborsGraph = GeneralDirectedGraph<int, float>()
generalDirectedNeighborsGraph.AddEdge(1, 2, 1.) |> ignore

let generalDirectedNeighborsSyncTrace = ResizeArray<int * int>()
generalDirectedNeighborsGraph.SendMessageToNeighbors(0, 1, fun count src dst _weight ->
    generalDirectedNeighborsSyncTrace.Add(src, dst)
    let nextCount = count + 1
    nextCount, nextCount >= 6)
expectTrace "General directed graph sync neighbor message passing should not bounce" [(1, 2)] generalDirectedNeighborsSyncTrace

let generalDirectedNeighborsAsyncTrace = ResizeArray<int * int>()
generalDirectedNeighborsGraph.SendMessageToNeighborsAsync(0, 1, fun count (src, dst, _weight) ->
    generalDirectedNeighborsAsyncTrace.Add(src, dst)
    let nextCount = count + 1
    nextCount, nextCount >= 6)
|> Async.RunSynchronously
expectTrace "General directed graph async neighbor message passing should not bounce" [(1, 2)] generalDirectedNeighborsAsyncTrace

let compressedDirectedChildrenGraph = CompressedDirectedGraph<int, float, int>(int)
compressedDirectedChildrenGraph.AddEdge(1, 2, 1.) |> ignore
compressedDirectedChildrenGraph.AddEdge(1, 3, 1.) |> ignore

let compressedDirectedChildrenSyncTrace = ResizeArray<int * int>()
compressedDirectedChildrenGraph.SendMessageToChildren(0, 1, fun count (src, dst, _weight) ->
    compressedDirectedChildrenSyncTrace.Add(src, dst)
    count + 1, true)
expectTrace "Compressed directed graph sync child message passing should stop globally" [(1, 2)] compressedDirectedChildrenSyncTrace

let compressedDirectedChildrenAsyncTrace = ResizeArray<int * int>()
compressedDirectedChildrenGraph.SendMessageToChildrenAsync(0, 1, fun count (src, dst, _weight) ->
    compressedDirectedChildrenAsyncTrace.Add(src, dst)
    count + 1, true)
|> Async.RunSynchronously
expectTrace "Compressed directed graph async child message passing should stop globally" [(1, 2)] compressedDirectedChildrenAsyncTrace

let compressedDirectedParentsGraph = CompressedDirectedGraph<int, float, int>(int, true)
compressedDirectedParentsGraph.AddEdge(1, 3, 1.) |> ignore
compressedDirectedParentsGraph.AddEdge(2, 3, 1.) |> ignore

let compressedDirectedParentsSyncTrace = ResizeArray<int * int>()
compressedDirectedParentsGraph.SendMessageToParents(0, 3, fun count (src, dst, _weight) ->
    compressedDirectedParentsSyncTrace.Add(src, dst)
    count + 1, true)
expectTrace "Compressed directed graph sync parent message passing should stop globally" [(3, 1)] compressedDirectedParentsSyncTrace

let compressedDirectedParentsAsyncTrace = ResizeArray<int * int>()
compressedDirectedParentsGraph.SendMessageToParentsAsync(0, 3, fun count (src, dst, _weight) ->
    compressedDirectedParentsAsyncTrace.Add(src, dst)
    count + 1, true)
|> Async.RunSynchronously
expectTrace "Compressed directed graph async parent message passing should stop globally" [(3, 1)] compressedDirectedParentsAsyncTrace

let compressedDirectedNeighborsGraph = CompressedDirectedGraph<int, float, int>(int)
compressedDirectedNeighborsGraph.AddEdge(1, 2, 1.) |> ignore

let compressedDirectedNeighborsSyncTrace = ResizeArray<int * int>()
compressedDirectedNeighborsGraph.SendMessageToNeighbors(0, 1, fun count (src, dst, _weight) ->
    compressedDirectedNeighborsSyncTrace.Add(src, dst)
    let nextCount = count + 1
    nextCount, nextCount >= 6)
expectTrace "Compressed directed graph sync neighbor message passing should not bounce" [(1, 2)] compressedDirectedNeighborsSyncTrace

let compressedDirectedNeighborsAsyncTrace = ResizeArray<int * int>()
compressedDirectedNeighborsGraph.SendMessageToNeighborsAsync(0, 1, fun count (src, dst, _weight) ->
    compressedDirectedNeighborsAsyncTrace.Add(src, dst)
    let nextCount = count + 1
    nextCount, nextCount >= 6)
|> Async.RunSynchronously
expectTrace "Compressed directed graph async neighbor message passing should not bounce" [(1, 2)] compressedDirectedNeighborsAsyncTrace

let compressedDirectedInEdgesGraph = CompressedDirectedGraph<int, float, int>(int, true)
compressedDirectedInEdgesGraph.AddEdge(4, 5, 2.5) |> ignore
expectEqual "Compressed directed graph in-edges should preserve reverse lookup" [|(4, 5)|] (compressedDirectedInEdgesGraph.InEdges 5)


let t = graphToTree gd ("C")

dispTree id t

let tvs, tes = toVerticesAndEdges t

let d = DirectedGraph<string>()

for v in tvs do d.AddNode v |> ignore

for n1, n2 in tes do
    match n1 with
    | Some parent -> d.AddEdge(parent, n2) |> ignore
    | None -> ()

disp2 d

GraphAlgorithms.isCyclic d
SimpleTrees.treeDepth 0 t
flattenWithShortPathBias t  

weightedGraphToTree g0 ("C", 0.)
//|> find (fst >> (=) "D") 
//|> dispTree string 
|> flattenWithShortPathBias

 
Branch("B", [Node "A"; Branch("C", [Node "D"; Branch("E", [Node "E1"])]); Node "F"])
//|> dispTree id
//|> find ((=) "E")  
|> toVerticesAndEdges

(*
let commaNumber (ToString str) = 
    let num,s = if str.[0] = '-' then str.[1..],"-" else str,""
    let decimpoint = let i = num.IndexOf(".") in if i = -1 then num.Length else i
    let npart , decimalpart = num.[..decimpoint-1], (num.[decimpoint..] )

    let num' = Seq.chunkBySize 3 (Seq.rev npart) |> Seq.map (Array.rev >> joinToString) |> Seq.rev |> joinToStringWith ","
    s + num' + (joinToString decimalpart)

*)

open Prelude.Math.Stats

let expectFailure label f =
    try
        f () |> ignore
        printfn "Expected failure for %s, but the call succeeded." label
    with ex ->
        printfn "Expected failure for %s: %s" label ex.Message

let dat = [2.,6.; 3.,8. ;12.,9.;5.,2.;16.,2.] 

dat |> List.fold online_covariance (0.,0.,0.,0.) 

dat |> List.unzip ||> simpleStats

dat |> List.map fst |> varianceAndMean 
dat |> List.averageBy snd



Array.collapseCols 
    [|  [|"A"; "B"|]
        [|"B"; "C"|]
        [|"A" ; "C"|]|]
    |> Array.map Seq.mode 
 
expectFailure "Array.splitEvenly uneven input" (fun () -> Array.splitEvenly 2 [|1..9|])

let thevec = [|for i in 0..15 -> random.NextDouble(1.,20.)|]
varianceAndMean thevec   
                            
let (v,m,n) = thevec |> Array.fold (fun (v,m,n) x -> online_variance_mean v m n x) (0.,0.,1.)
v/(n-1.), m

///////


timedn 3 {yield 3}

timed { yield 3 } 

timed {for _ in 1..2 -> 3} 

timeds false 3 {for _ in 1..3 -> 3} 

timeds false 0 {for _ in 1..3 -> 3} 

/////////////////////////
Array.rot 1 [|1..3|] = [|3;1;2|]
Array.rot 2 [|1..3|] = [|2;3;1|]
Array.rot 3 [|1..3|] = [|1;2;3|]
Array.rot 4 [|1..3|] = Array.rot 1 [|1..3|]  
Array.rot -1 [|1..3|] = Array.rot 2 [|1..3|] 
Array.rot -2 [|1..3|] = Array.rot 1 [|1..3|]
Array.rot -3 [|1..3|] = Array.rot 3 [|1..3|] 
Array.rot -4 [|1..3|] = Array.rot -1 [|1..3|] 
/////////////
String.splitSentenceManual "ye. water will be 4. dollars.I say\n yes Mr. fred it's .5 to U.C.L.A. and has a Ph.D. And this is a legit sentence too."
//////////////////////

["Apple" ; "BEE"; "CAT"; "Dog"; "elephant"] |> List.map (fun (LowerCase w) -> w)
["Apple" ; "BEE"; "CAT"; "Dog"; "elephant"] |> List.map (fun (UpperCase w) -> w)

///////////////////////
exponentialAverage id 0.45  None [20. ; 10. ; 40. ; 10.; 15.; 20.; 500. ; 500.; 800.]

[20. ; 10. ; 40. ; 10.; 15.; 20.; 500. ; 500.; 800.] |> List.fold (exponentialSmoothing id 0.3) 30. 

///
hoursToText 5.67
hoursToText (25.02)
hoursToText (2. * 168. + 24.)
hoursToText 24.

//////

DateTime.Now.AddDays(-54.).StartOfMonth()
                       

Seq.findIndexi (fun _ c -> c = 'f') "fright"
Seq.findIndexi (fun i c -> i >= 4 && c = 'f') "fright"

////////////

String.longestCommonSubstring "apple" "appetitie"
Array.longestCommonSubvec [|1;2;3;2;3|] [|4;2;3|]   
String.longestCommonSubstring "airtight" "foghorn" 
String.longestCommonSubstring "airtight" "failure" 
String.longestCommonSubSeq "airtight" "foghorn"

String.longestCommonSubSeq "airtight" "failure"
String.readLongestCommonSubSeqResult (String.longestCommonSubSeq "abc" "axbyc") = "abc"
      
Seq.longestCommonSubSeq [1..9] [2..2..20]

String.longestCommonSubSeq "airtight" "failure"

String.splitNatATime 2 "cattarang"

////

let wg = WeightedGraph<string>()

wg.AddNode("a") |> ignore
wg.AddNode("b") |> ignore
wg.AddNode("c") |> ignore
wg.AddEdge("a","b", 2.) |> ignore
wg.AddEdge("c","b", 2.) |> ignore

wg.AdjustWeight ("a", "b", (+) 1.) 

wg
////


///////////

Array.filteriMap (fun i x -> i + 3 < x && x % 2 = 0) squared [|0..2..9|]
[|0..2..9|] |> Array.mapi Tuple.pair |> Array.filter (fun (i,x) -> i + 3 < x && x % 2 = 0) |> Array.map (snd >> squared)

//Array.mapFilteri squared (fun i x -> i + 3 < x  && x % 2 = 0)  [|0..2..9|]


let z = Array.mapi Tuple.pair  [|0..2..9|]
///////////
  
"A CHARACTERIZATION OF ENTROPY IN TERMS OF INFORMATION LOSS" |> String.tolower |> String.capitilizebySpace 2

/////
let teststr0 = "ab"
let teststr1 = "abcdef"

let padtst n f s1 s2 = "\n" + (f n s1) + " efd" + "\n" + (f n s2) + " efd"

padtst 2 String.padcut teststr0 teststr1
teststr1.Length 

let tststr = "This is \"number\" five's test"

tststr.Replace("'", "[apos]").Replace("\"", "[qu]").Replace (String.newLine, "")
String.replaceMultiple [|"'", "[apos]"; "\"", "[qu]"; String.newLine,""|] tststr

String.transformMultiple Text.RegularExpressions.Regex.Escape [|"(cat)"; "dog"|] "The animal (cat) slapped the dog"

/////
//= [|2; 3; 7|]
[|[|2; 5; 10|]
  [|1; 2; 3|]
  [|3; 2; 8|]|] |> Array.colAverageFloats 

////
let trieDict = dict_as_trie [|"apple"; "app" ;"art"; "cat"; "card"; "carded"; "cap"|]

autocomplete 2 trieDict "ca" 

//

[1..10] |> Seq.takeOrMax 40 |> Seq.toArray

[1..100] |> Seq.filterMapTake 3 ((flip (%) 2) >> ((=) 0)) ((*) 2)
  
/////Map Merging

let m1 = Map.ofList [1,2; 2,3]
let m2 = Map.ofList [1,5; 2,2; 3,7; 9,91]

let d1 = Dict.ofIDict m1
let d2 = Dict.ofIDict m2

d1.MergeWith ((-), d2)
d1.MergeWith (konst, d2)

d1 |> Seq.toArray |> Array.map keyValueToPair

let m3 = Map.merge (+) id m1 m2
let m4 = Map.merge (+) id m2 m1 
m3 = m4 //true
m3 = Map.ofList [1,7; 2,5; 3,7; 9,91]

let m5 = Map.merge (-) id m1 m2
let m6 = Map.merge (-) id m2 m1 
m5 = m6 //false not commutative 
m5 |> Map.map (fun _ x -> abs x) = (m6 |> Map.map (fun _ x -> abs x))
 
//Folds right?
let mDict = Dict.ofSeq [(4,1); (1,3); (2,1)] 

mDict.FoldValues(0, (+)) //=5

mDict.FoldKeyValues(0, fun s (DictKV(k, v)) -> s + k + v) //12

mDict.FoldKeyValues("", fun s (DictKV(_, v)) -> s + string v) <> "311"

mDict.FoldKeyValues(0, fun s (DictKV(k, v)) -> s + k + v) = 12
/////////////
[0..10] |> Seq.takeOrMax 2 |> Seq.length = 2
[0..10] |> Seq.takeOrMax 200 |> Seq.length = 11

///////Testing Reducers /////////
//Example - wordcount

let fhackPath = System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments) + "\\fhack.txt"

if System.IO.File.Exists fhackPath then
    let lines = System.IO.File.ReadAllLines fhackPath
    timeThis 100 (fun () ->
        let c =
            lines
            |> Reducer.toSeqReducer
            |> Reducer.collect (fun line -> Reducer.toSeqReducer <| (line |> String.splitToWords))
            |> Reducer.countBy id
        c |> Seq.toArray)
    |> ignore
else
    printfn "Skipping reducer wordcount sample; file not found: %s" fhackPath
// |> Reducer.groupBy id (fun _ -> 1) (fun (_, items) -> Seq.sum items)

///////////////

String.containsNof 1 [|"apple"; "tree"|] "jumped off the tree"
String.containsNof 2 [|"apple"; "tree"|] "jumped off the tree"
String.containsNof 2 [|"apple"; "tree"|] "jumped off the apple tree"
String.containsNof 2 [|"apple"; "tree"; "plum"|] "jumped off the plum tree"

/////////////
String.removeExtrasOfString String.newLine (sprintf "hello%s%sthere%s%s%syes" String.newLine String.newLine String.newLine String.newLine String.newLine)

String.removeExtrasOfString "<br/><br/>" "hey<br/>there<br/><br/>now<br/><br/><br/>oh"

"hey<br/>there<br/><br/>now<br/><br/><br/><br/><br/>oh".Splitby ("<br/><br/>")  |> String.joinWith "<br/><br/>"
 


////////////////Testing Threadsafe random numbers
let inf x =
    let var, mean= x |> varianceAndMean
    let minx,maxX = x |> Array.min, x |> Array.max 
    var, mean, minx, maxX

timeThis 1 (fun () -> Threading.Tasks.Parallel.For(0, 50000000, (fun  _ -> RandomX.Next() |> ignore)) |> ignore )

let numParallel = [|0..20000|] |> Array.Parallel.map (fun _ -> RandomX.NextDouble(-100000.,100000.))
let numSeq = [|0..20000|] |> Array.map (fun _ -> random.NextDouble(-100000.,100000.))

let datPar = numParallel |> Array.collect BitConverter.GetBytes
let datSeq = numSeq |> Array.collect BitConverter.GetBytes

let h = Hashset numParallel
let h2 = Hashset numSeq

h.Count, h2.Count
h.IntersectWith h2
h.Count
//test quality by compressing using 7zip, settings dont matter really, e.g true size 157KB -> 147 KB
//threadsafe and regular compress the same amount. Random data should not be very compressible, that it is would indicate bad statistical propeties.
//Very compressible is very predictable. To show bad behevior make a static 
//random variable in the randomX class rather than use the static 'random' defined globally.
IO.File.WriteAllBytes(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)+"\\testRandomPar.dat",datPar)
IO.File.WriteAllBytes(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)+"\\testRandomSeq.dat",datSeq)
inf numParallel 
inf numSeq

//bucketing
bucketRange 0 5. 2. = 0.
bucketRange 0 5. 40. = 40.
bucketRange 1 0.5 1.6 = 1.5

/////contain
String.containsAll [|"apple"; "bag"; "key"|] "apple bag key" 
String.containsAll [|"apple"; "bag"; "key"|] "applebagkey" 
String.containsAll [|"apple"; "bag"; "key"|] "applbagkey"  

String.containsOneOf [|"apple"; "bag"; "key"|] "apple bag key" 
String.containsOneOf [|"apple"; "bag"; "key"|] "applebagkey" 
String.containsOneOf [|"apple"; "bag"; "key"|] "applbagkey" 
String.containsOneOf [|"apple"; "bag"; "key"|] "applbigkay" 

/////////
// From: http://stackoverflow.com/questions/286427/calculating-permutations-in-f
let rec internal insertions x = function
    | []  -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

let genPermutations collection = 
    let rec permutations  = function
        | []      -> seq [ [] ]
        | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs)) 
    collection |> Seq.toList |> permutations 
     

////////////////
open Prelude.Sampling
 
cdf [|"a",0.5;"b",0.2;"c",0.3|]

[| for _ in 0..9999 -> discreteSample [|"a",5.;"b",2.;"c",2.5; "d", 0.5|] |]


open Prelude.Control
//open FSharp.Control.Reactive

let mm = StateMachineExec<string,int>("Q") 

 
let A c = 
    printfn "In A: %A" c
    if c <= 5 then 
        {NextState = "B"; Mem = c + 1}
    else {NextState = "Q"; Mem = c}
 

let B (c) = 
    printfn "In B: %A" c 
    {NextState = "A"; Mem = c + 1}
         

mm.Register("A", A)
mm.Register("B", B)

mm.Post({NextState = "A"; Mem = 0})

type SimpleChildState =
    | Step1
    | Step2

type SimpleHfsmState =
    | Working of SimpleChildState
    | Finished

let simpleHfsm = StateMachineExec<SimpleHfsmState, int>(Finished)

simpleHfsm.Register(
    Working Step1,
    fun count ->
        printfn "Exact child handler: Step1, count = %d" count
        { NextState = Working Step2; Mem = count + 1 })

simpleHfsm.RegisterCase(
    (function
     | Working child -> Some child
     | _ -> None),
    fun child count ->
        printfn "Parent handler: %A, count = %d" child count
        match child with
        | Step1 -> { NextState = Working Step2; Mem = count + 1 }
        | Step2 -> { NextState = Finished; Mem = count + 1 })
|> ignore

simpleHfsm.Post({ NextState = Working Step1; Mem = 0 })

type FrameSliceState =
    | Acquire
    | Pathfind
    | Move
    | Done

let frameSliced = SteppableStateMachineExec<FrameSliceState, int, unit>(Done)

frameSliced.RegisterOutcome(
    Acquire,
    fun budget ->
        printfn "Acquire slice: %d" budget
        Yield { NextState = Pathfind; Mem = budget + 1 })

frameSliced.Register(
    Pathfind,
    fun budget ->
        printfn "Pathfind slice: %d" budget
        { NextState = Move; Mem = budget + 1 })

frameSliced.Register(
    Move,
    fun budget ->
        printfn "Move slice: %d" budget
        { NextState = Done; Mem = budget + 1 })

let firstSlice = frameSliced.RunSingleStep({ NextState = Acquire; Mem = 0 })
printfn "First slice stop = %A, pending = %A" firstSlice.StopReason firstSlice.PendingTransition

let secondSlice = frameSliced.StepFor(1)
printfn "Second slice stop = %A, pending = %A" secondSlice.StopReason secondSlice.PendingTransition

let finalSlice = frameSliced.StepCurrent()
printfn "Final slice stop = %A" finalSlice.StopReason

let untilMove =
    frameSliced.RunUntil(
        { NextState = Pathfind; Mem = 10 },
        function
        | Some(Move, _) -> true
        | _ -> false)

printfn "RunUntil stop = %A, pending = %A" untilMove.StopReason untilMove.PendingTransition

//////////////////////

