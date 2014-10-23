#load "./prelude/prelude.fs"         
#load "./prelude/simplegraph.fs"
#load "./prelude/math.fs"
#load "./prelude/PriorityQueues.fs"
#load "./prelude/fibonacciheap.fs"
#time "on"
open System
open Prelude.Common
open Prelude.SimpleGraphs
open Prelude.Math

[<Struct;CustomComparison;CustomEquality>]                              
type WeightPair<'a when 'a: comparison> = 
  val Weight : float; val X : 'a ; val Y:'a      
  new(w:float,x,y) = 
    {Weight = w; X = x; Y = y} 
   
  override x.ToString() = (string x.Weight) + ", " + (x.X.ToString()) + "," + (x.Y.ToString())
 
  interface IEquatable<WeightPair<'a>> with
        member this.Equals(other) =
            this.Weight = other.Weight && this.X = other.X && this.Y = other.Y

  interface IComparable<WeightPair<'a>> with
        member this.CompareTo(other) =
          if this.Weight = other.Weight then
            compare (this.X, this.Y) (other.X, other.Y)
          else this.Weight.CompareTo(other.Weight) 

[<Struct;CustomComparison;CustomEquality>]                              
type WeightPart<'a when 'a: comparison> = 
  val Weight : float; val X : 'a       
  new(w:float,x) = 
    {Weight = w; X = x} 
   
  override x.ToString() = (string x.Weight) + ", " + (x.X.ToString()) 
 
  interface IEquatable<WeightPart<'a>> with
        member this.Equals(other) =
            this.Weight = other.Weight && this.X = other.X  

  interface IComparable<WeightPart<'a>> with
        member this.CompareTo(other) =
          if this.Weight = other.Weight then
            compare this.X other.X 
          else this.Weight.CompareTo(other.Weight) 

open System.Collections.Generic
type WeightedGraph<'a when 'a: equality and 'a:comparison>() = 
    let edges = Dict<'a, Hashset<WeightPart<'a>>>() 
    
    member x.EdgeData = edges 
    member x.InsertVertex(s:'a) =  
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s,Hashset()) 
        contained
    
    member x.Remove(v:'a) = 
       match (edges.tryFind v) with
        | None -> false
        | Some elist ->                                         
           elist |> Seq.iter (fun ((weight_v2)) -> edges.[weight_v2.X].Remove(weight_v2) |> ignore)
           edges.Remove v

    member x.InsertEdge (v0,v1, w) = 
        maybe {
        let! elist0 = edges.tryFind v0
        let! elist1 = edges.tryFind v1

        let added1 = elist0.Add (WeightPart(w, v1))
        let added2 = elist1.Add (WeightPart(w, v0))
       
        return (added1,added2)}  

    member x.ContainsVertex v = edges.ContainsKey v 

    member x.ContainsEdge v1 v2 = 
        maybe {
        let! elist0 = edges.tryFind v1 
        return (elist0.Contains v2)}  

    member x.GetEdges v = 
        maybe {
         let! elist = edges.tryFind v  
         return elist }

    member x.Edges = 
      Hashset(x.EdgeData
          |> Seq.collect (fun (DictKV(k,v)) -> 
                                v |> Seq.map (fun (w_v2) -> lessToLeft(k, w_v2.X), w_v2.Weight) 
                                  |> Seq.toArray) 
          |> Seq.toArray) |> Seq.toArray

    member x.OrderedEdges =
      let sorted = Collections.Generic.SortedSet()    
      x.EdgeData
      |> Seq.iter 
         (fun (DictKV(k,v)) -> 
           v |> Seq.iter 
                (fun (w_v2) -> 
                  sorted.Add (WeightPart(w_v2.Weight, lessToLeft(k, w_v2.X))) 
                  |> ignore  )) 
      sorted
    member x.Vertices = Hashset(edges.Keys)

    member x.MinimumSpanningTree() = 
            let currentCut = Hashset(edges.Keys)
            let root = currentCut |> Seq.head
            let tree = FastGraphGeneric()
            let fs = Collections.Generic.SortedSet()    

            let  _, _, steps =
              recurse (fun _ -> currentCut.Count = 0)
                    (fun (v, i, getnodes) -> 
                          //printfn "at node %A" v
                          if getnodes then
                            //printfn "current cut %A\n" currentCut
                            edges.[v] 
                            |> Seq.iter (fun (w_v2) ->                                                    
                                  if currentCut.Contains w_v2.X || currentCut.Contains v then   
                                    fs.Add (WeightPair(w_v2.Weight,v,w_v2.X)) 
                                    ()) 
                          let v0,v2, _, next = 
                              let minel = fs.Min in minel.X, minel.Y, minel.Weight, minel
                          //printfn "New edges: %A\nAvailable: %A" (edges.[v] |> Seq.toArray) (fs |> Seq.toArray) 
                          //printfn "selecting %A" next
                            
                          fs.Remove next

                          if (currentCut.Contains v0 || currentCut.Contains v2) then 
                            let vin0 = tree.InsertVertex v0
                            let vin = tree.InsertVertex v2
                            let nedge = tree.InsertEdge (v0, v2)
                            //printfn "new v in already? %A, v0? %A new e? %A" vin0 vin nedge
                            currentCut.Remove v0 
                            currentCut.Remove v2 
                            (v2, i + 1, true)
                          else                           
                            //printfn "node fail"
                            (v, i + 1, false) 
                      ) (root, 0, true)     
            //printfn "%d steps" steps
            tree
////////////////////////////////

let wg = WeightedGraph<string> ()  

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

wg.InsertVertex "a"
wg.InsertVertex "b"
wg.InsertVertex "c"
wg.InsertVertex "d"

wg.InsertEdge ("a", "b", 2.)
wg.InsertEdge ("a", "d", 1.)
wg.InsertEdge ("b", "d", 2.)
wg.InsertEdge ("c","d", 3.)   

////spanning tree
wg.InsertVertex "a"
wg.InsertVertex "b"
wg.InsertVertex "c"
wg.InsertVertex "d"
wg.InsertVertex "e"
wg.InsertVertex "f"

wg.InsertEdge ("a", "b", 1.)
wg.InsertEdge ("a", "d", 4.)
wg.InsertEdge ("a", "e", 3.)
wg.InsertEdge ("b","d", 4.)
wg.InsertEdge ("b", "e", 2.)
wg.InsertEdge ("d", "e", 4.)
wg.InsertEdge ("e", "f", 7.)
wg.InsertEdge ("c","f", 5.)
wg.InsertEdge ("c","e", 4.)
                                                      
wg.MinimumSpanningTree()    

wg.InsertVertex "a"
wg.InsertVertex "b"
wg.InsertVertex "c"
wg.InsertVertex "d"
wg.InsertVertex "e"
wg.InsertVertex "f"
wg.InsertVertex "g"
                          
wg.InsertEdge ("a", "b", 4.)
wg.InsertEdge ("a", "c", 8.)
wg.InsertEdge ("b", "c", 9.)
wg.InsertEdge ("b","d", 8.)
wg.InsertEdge ("d", "c", 2.)
wg.InsertEdge ("d", "f", 9.)
wg.InsertEdge ("c", "f", 1.)
wg.InsertEdge ("f","e", 5.)
wg.InsertEdge ("f","g", 2.)
wg.InsertEdge ("e","g", 6.)
wg.InsertEdge ("b","e", 10.)
wg.InsertEdge ("d","e", 7.)

let wgr = WeightedGraph<int>()      
                                   
//with pque 13 secs | set 7 sec | .net sortedset 6 sec
//sorted dict specializedd: 4 second | key val pque: 3 secs
//sorted set with 1-15 edges 17 secc | key val pq: 8 secs
//final 5 sec
for x in 0..100000 do
    wgr.InsertVertex x
    for j in 0..(max (-1)) (min (x-1) (random.Next(1,15))) do
       wgr.InsertEdge (x, random.Next(0,x - 1), random.NextDouble(1.,5.))
       ()                                        
               
//with pq 10 secs | set 14 sec |sorted set 5 sec | keyval pque 4.6 sec  | 6 secs
//1-15 edges: 10 sec | kv pq: 9 sec | final - correct: 16 secs 

//using a struct makes all the difference. with a sorted list best balance.
let c = wgr.MinimumSpanningTree()  
  
wgr.EdgeData |> Seq.map (keyValueToValue >> Seq.length >> float) |> Seq.toArray  
                                            
c.Edges.Length
b.Edges= c.Edges

//////////////////////////

open Prelude.Collections.FibonacciHeap

let dijkstra (g:WeightedGraph<'a>) source target = 
     let dists = Dict.ofSeq [source, 0.] 
     let prev = Dict()
     let vs = g.Vertices
     let q = FibHeap.create ()
     let visited = Hashset()
     let nodeMap = Dict()
                         
     for v in vs do
       if v <> source then dists.Add(v, Double.MaxValue)
       prev.Add(v, None) |> ignore
       let n = FibHeap.insert_data q v dists.[v]
       nodeMap.Add(v, n)
     
     recurse 
      (fun stop -> stop || FibHeap.size q <= 0)
      (fun _ -> 
        let next = FibHeap.extract_min_data q
        printfn "select: %A" next
        if next = target then printfn "found target" ; true
        else 
          let adjs = g.GetEdges next
          visited.Add next
                             
          printfn "near: %A" adjs
                
          match adjs with 
            | None -> false
            | Some vs ->  
              vs |> Seq.iter (fun v2 -> 
                if not (visited.Contains v2.X) then
                  let alt = dists.[next] + v2.Weight
                  printfn "current: %A, %A changed:%A "v2.X dists.[v2.X] alt
                  if alt < dists.[v2.X] then 
                    dists.[v2.X] <- alt
                    prev.[v2.X] <- Some next
                    FibHeap.decrease_key q nodeMap.[v2.X] alt)
              false) (false)  
      
     recurse (fst >> Option.isNone)
             (fun (Some p,l) -> 
                printfn "current node: %A" p
                printfn "current path: %A" l   
                prev.getOrDef p None, p::l) (Some target, [])     

(*     1
   A -------B
  1|   \  9 | 1
   |_____\ C
  E    4   | 1.
           D  *)

let wp = WeightedGraph<string> ()

wp.InsertVertex "a"
wp.InsertVertex "b" 
wp.InsertVertex "c"
wp.InsertVertex "d"
wp.InsertVertex "e"

wp.InsertEdge ("a", "b", 1.)  
wp.InsertEdge ("b", "c", 1.)
wp.InsertEdge ("c", "d", 1.)
wp.InsertEdge ("a", "e", 1.)
wp.InsertEdge ("e", "c", 4.)  
wp.InsertEdge ("a", "c", 9.)  

dijkstra wp "a" "e"
dijkstra wp "a" "c"
dijkstra wp "a" "d"
dijkstra wp "b" "e"
dijkstra wp "d" "e"  

dijkstra wp "a" "f" 

dijkstra wgr 5 44440

wgr.Edges.Length
   
//let sg =  UndirectedGraph<int, Edge<int>>()
//1 sec | with 1,15 edges: 4 secs 
//for x in 0..100000 do
//    sg.AddVertex x
//    for j in 0..(max (-1)) (min (x-1) (random.Next(1,15))) do
//       sg.AddEdge (Edge(x, random.Next(0,x - 1)))   
//       ()
//3 sec  | 5 sec
//sg.MinimumSpanningTreePrim(fun _ -> random.NextDouble(1., 5.))
//
//let sg =  UndirectedGraph<string,WeightedEdge<string>>()
//sg.AddVertex "a"
//sg.AddVertex "b"
//sg.AddVertex "c"
//sg.AddVertex "d"
//sg.AddVertex "e"
//sg.AddVertex "f"
//
//sg.AddEdge (WeightedEdge("a", "b", 1.))
//sg.AddEdge (WeightedEdge("a", "d", 4.))
//sg.AddEdge (WeightedEdge("a", "e", 3.))
//sg.AddEdge (WeightedEdge("b","d", 4.))
//sg.AddEdge (WeightedEdge("b", "e", 2.))
//sg.AddEdge (WeightedEdge("d", "e", 4.))
//sg.AddEdge (WeightedEdge("e", "f", 7.))
//sg.AddEdge (WeightedEdge("c","f", 5.))
//sg.AddEdge (WeightedEdge("c","e", 4.))
//
//let ng = sg.MinimumSpanningTreePrim(fun e -> e.Weight)
//ng |> Seq.toArray |> Array.map (fun e -> e.Source, e.Target, e.Weight)


///////////

//    member x.MinimumSpanningTree() = 
//            let vertices = edges.Keys |> Set.ofSeq
//            let root = Set.minElement vertices       
//            ////printfn "%A" root
//            let tree = FastGraphGeneric()
//        
//            let weightDict = edges |> Seq.map (fun (KeyValue(k,sq)) -> k, Collections.Generic.SortedSet(sq)) 
//                                   |> Dict.ofSeq 
//
//            let _, _, steps =
//              recurse (fst3 >> Set.isEmpty)
//                    (fun (currentCut,v, i) -> 
//                          let elist = weightDict.[v] 
//                          //printfn "checking %A " v   
//
//                          match (elist.Count = 0) with 
//                           | true -> tree.InsertVertex v |> ignore //if v is root and not connected to anything
//                                     //printfn "not found"
//                                     let nextCut = currentCut.Remove v
//                                     if Set.isEmpty nextCut then (Set.empty , v, i) 
//                                     else (nextCut, nextCut.MinimumElement, i + 1)
//                           | false  ->    
//                              let (w,v2,wvec) = let wv = elist.Min in wv.Weight, wv.Thing, wv
//                              elist.Remove (wvec) 
//                              //printfn "Check cut contains %A %A, both %A, %A in tree: %A" v (currentCut.Contains v) v v2 ((tree.ContainsVertex v && tree.ContainsVertex v2))
//                              if currentCut.Contains v && not(tree.ContainsVertex v && tree.ContainsVertex v2)
//                                  then
//                                tree.InsertVertex v |> ignore
//                                tree.InsertVertex v2  |> ignore
//                                tree.InsertEdge(v, v2) |> ignore
//                                weightDict.[v2].Remove(wvec)
//                                ()
//                                //printfn "candidate found" 
//                              //printfn "Name: %A | Weight: %A" v2  w
//                              //printfn "%A list %A" v (edges.[v] |> Seq.toArray) 
////                              ////printfn "removing %A" v
//                             // x.Remove v |> ignore
//
//                               // remove the popped 
//                              ////printfn "v contained? %A" (x.ContainsVertex v)
//                              //printfn "%A" (edges.[v2] |> Seq.toArray)   
//                              (currentCut.Remove v, v2, i + 1)) (vertices,root, 0)     
//            //printfn "%d steps" steps
//            tree