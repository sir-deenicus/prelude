#load "./prelude/prelude.fs"         
#load "./prelude/simplegraph.fs"
#load "./prelude/math.fs"
#time "on"
open System
open Prelude.Common
open Prelude.SimpleGraphs
open Prelude.Math

[<Struct;CustomComparison;CustomEquality>]                              
type WeightPair<'a when 'a: equality> = 
  val Weight : float; val Thing : 'a          
  new(w:float,thing) = 
    {Weight = w; Thing = thing} 
   
  interface IEquatable<WeightPair<'a>> with
        member this.Equals(other) =
            this.Weight = other.Weight && this.Thing = other.Thing
             
  interface IComparable<WeightPair<'a>> with
        member this.CompareTo(other) =
            this.Weight.CompareTo(other.Weight)

type WeightedGraph<'a when 'a: equality and 'a:comparison>() = 
    let edges = Dict<'a, Collections.Generic.SortedSet<WeightPair<'a>>>() 
    
    member x.EdgeData = edges 
    member x.InsertVertex(s:'a) =  
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s,Collections.Generic.SortedSet()) 
        contained
    
    member x.Remove(v:'a) = 
       match (edges.tryFind v) with
        | None -> //printfn "not found"
                  false
        | Some elist -> 
//           printfn "removing, %A" (elist.GetInternalList())
           elist |> Seq.iter (fun ((weight_v2)) -> edges.[weight_v2.Thing].Remove((weight_v2)) |> ignore)
           edges.Remove v

    member x.InsertEdge (v0,v1, w) = 
        maybe {
        let! elist0 = edges.tryFind v0
        let! elist1 = edges.tryFind v1

        elist0.Add (WeightPair(w, v1))
        elist1.Add (WeightPair(w, v0))
       
        return true}  

    member x.ContainsVertex v = edges.ContainsKey v 

    member x.ContainsEdge v1 v2 = 
        maybe {
        let! elist0 = edges.tryFind v1 
        return (elist0.Contains v2)}  

    member x.GetEdges v = 
        maybe {
         let! elist = edges.tryFind v  
         return elist }

    member x.MinimumSpanningTree() = 
            let vertices = edges.Keys |> Set.ofSeq
            let root = Set.minElement vertices       
            //printfn "%A" root
            let tree = FastGraphGeneric()
        
            let weightDict = edges |> Seq.map (fun (KeyValue(k,sq)) -> k, Collections.Generic.SortedSet(sq)) 
                                   |> Dict.ofSeq 

            recurse (fst3 >> Set.isEmpty)
                    (fun (currentCut,v, i) -> 
                          let elist = weightDict.[v] 
                          //printfn "checking %A " v   

                          match (elist.Count = 0) with 
                           | true -> tree.InsertVertex v |> ignore //if v is root and not connected to anything
                                     //printfn "not found"
                                     let nextCut = currentCut.Remove v
                                     if Set.isEmpty nextCut then (Set.empty , v, i) 
                                     else (nextCut, nextCut.MinimumElement, i + 1)
                           | false  ->    
                              let (w,v2,wvec) = let wv = elist.Min in wv.Weight, wv.Thing, wv
                              elist.Remove (wvec)  
                              if i = 0 || (tree.ContainsVertex v && not (tree.ContainsVertex v2))
                                || (tree.ContainsVertex v2 && not (tree.ContainsVertex v)) then
                                tree.InsertVertex v |> ignore
                                tree.InsertVertex v2  |> ignore
                                tree.InsertEdge(v, v2) |> ignore
                                //printfn "added" 
                              //printfn "found %A " v2
                              //printfn "%A list %A" v (edges.[v] |> Seq.toArray) 
//                              //printfn "removing %A" v
                             // x.Remove v |> ignore

                              weightDict.[v2].Remove(wvec) // remove the popped 
                              //printfn "v contained? %A" (x.ContainsVertex v)
                              //printfn "%A" (edges.[v2] |> Seq.toArray)   
                              (currentCut.Remove v, v2, i + 1)) (vertices,root, 0) |> ignore
                    
            tree



let wg = WeightedGraph<string>()

wg.InsertVertex "a"
wg.InsertVertex "b"
wg.InsertVertex "c"
wg.InsertVertex "d"

// D
//|   
//C -  A - B
wg.InsertEdge ("a", "b", 1.)
wg.InsertEdge ("a", "c", 2.)
wg.InsertEdge ("c","d", 2.)
wg.EdgeData 

wg.Remove "a"
(wg.GetEdges "b") 

//wiki edges prim

wg.InsertEdge ("a", "b", 2.)
wg.InsertEdge ("a", "d", 1.)
wg.InsertEdge ("b", "d", 2.)
wg.InsertEdge ("c","d", 3.)

wg.MinimumSpanningTree()

let wgr = WeightedGraph<int>()      
                                   
//with pque 13 secs | set 7 sec | .net sortedset 6 sec
//sorted dict specializedd: 4 second | key val pque: 3 secs
//sorted set with 1-15 edges 17 secc | key val pq: 8 secs
for x in 0..100000 do
    wgr.InsertVertex x
    for j in 0..(max (-1)) (min (x-1) (random.Next(1,15))) do
       wgr.InsertEdge (x, random.Next(0,x - 1), random.NextDouble(1.,5.))
       ()

//with pq 10 secs | set 14 sec |sorted set 5 sec | keyval pque 4.6 sec
//1-15 edges: 10 sec | kv pq: 9 sec    

//using a struct makes all the difference. with a sorted list best balance.
                                          
let c = wgr.MinimumSpanningTree()    
  
b.Edges.Length
b.Edges= c.Edges

//////////////////////////
//let sg =  UndirectedGraph<int, Edge<int>>()
////1 sec | with 1,15 edges: 4 secs 
//for x in 0..100000 do
//    sg.AddVertex x
//    for j in 0..(max (-1)) (min (x-1) (random.Next(1,15))) do
//       sg.AddEdge (Edge(x, random.Next(0,x - 1)))   
//       ()
////3 sec  | 5 sec
//sg.MinimumSpanningTreePrim(fun _ -> random.NextDouble(1., 5.))