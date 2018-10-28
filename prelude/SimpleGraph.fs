module Prelude.SimpleGraphs

open Prelude.Common
open System
open Prelude.Collections.FibonacciHeap
//open Prelude.Collections

[<AbstractClass>]
type FastGraph<'a when 'a: comparison>() =
    abstract member InsertVertex : 'a -> bool
    abstract member InsertEdge : 'a * 'a ->  (bool * bool) option
    abstract member ContainsVertex : 'a -> bool
    abstract member EdgeData : Collections.Generic.IDictionary<'a, 'a Hashset>
    abstract member ContainsEdge : 'a -> 'a -> bool option
    abstract member GetEdges : 'a -> 'a [] option 

    member x.Vertices = x.EdgeData |> Seq.map keyValueToKey |> Seq.toArray
   
    member x.Edges = Hashset(x.EdgeData
                        |> Seq.collect (fun (DictKV(k,v)) -> 
                                              v |> Seq.map (fun e2 -> lessToLeft (k, e2)) 
                                                |> Seq.toArray) 
                        |> Seq.toArray) |> Seq.toArray
   

type FastStringGraph() =  
    inherit FastGraph<string>()
    let edges = Dict<string,string Hashset>() 
    
    override x.InsertVertex(s:string) =  
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s, Hashset()) 
        contained
    
    member x.Remove(v) = 
       match (edges.tryFind v) with
        | None -> false
        | Some elist -> elist |> Seq.iter (fun v2-> edges.[v2].Remove(v) |> ignore)
                        edges.Remove v

    override x.EdgeData = edges :> Collections.Generic.IDictionary<string, string Hashset>
          
    override x.InsertEdge (v0,v1) = 
        maybe {
        let! elist0 = edges.tryFind v0
        let! elist1 = edges.tryFind v1

        let in0 = elist0.Add v1
        let in1 = elist1.Add v0
       
        return (in0,in1)}  

    override x.ContainsVertex v = edges.ContainsKey v

    override x.ContainsEdge v1 v2 = 
        maybe {
        let! elist0 = edges.tryFind v1 
        return (elist0.Contains v2)}  

    override x.GetEdges v = 
        maybe {
          let! elist = edges.tryFind v  
          return elist |> Seq.toArray } 


type FastGraphGeneric<'a when 'a: equality and 'a:comparison>() = 
    inherit FastGraph<'a>() 
    let edges = Dict<'a,'a Hashset>()    
    
    override x.EdgeData = edges :> Collections.Generic.IDictionary<'a, 'a Hashset>
    override x.InsertVertex(s:'a) =  
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s, Hashset()) 
        contained

    member x.Remove(v:'a) = 
       match (edges.tryFind v) with
        | None -> false
        | Some elist -> elist |> Seq.iter (fun v2-> edges.[v2].Remove(v) |> ignore)
                        edges.Remove v

    override x.InsertEdge (v0,v1) = 
        maybe {
        let! elist0 = edges.tryFind v0
        let! elist1 = edges.tryFind v1

        let in0 = elist0.Add v1
        let in1 = elist1.Add v0
       
        return (in0,in1)}  

    override x.ContainsVertex v = edges.ContainsKey v 

    override x.ContainsEdge v1 v2 = 
        maybe {
        let! elist0 = edges.tryFind v1 
        return (elist0.Contains v2)}  

    override x.GetEdges v = 
        maybe {
        let! elist = edges.tryFind v  
        return elist |> Seq.toArray } 


///uses integer list for edges, better for Dense graphs.
type FastStringGraph2() = 
   let vertdata = MutableList<string>()
   let edges = Dict<string,int * int Hashset>()  

   member x.InsertVertex(s:string) =  
     let contained = edges.ContainsKey s
     if not contained then edges.Add(s, (vertdata.Count, Hashset())); vertdata.Add s
     contained 

   member x.InsertEdge (v1,v2) = 
     maybe {
      let! i0, elist0 = edges.tryFind v1
      let! i1, elist1 = edges.tryFind v2

      let edgeList = elist0.Add i1
      let edgeList2 = elist1.Add i0
       
      return true}  
  
   member x.ContainsVertex v = edges.ContainsKey v

   member x.ContainsEdge v1 v2 = 
     maybe {
      let! _, elist0 = edges.tryFind v1
      let! i1, _ = edges.tryFind v2 
       
      return (elist0.Contains i1)}  

   member x.GetEdges v = 
     maybe {
      let! _, elist = edges.tryFind v  
      return elist |> Seq.map (fun i -> vertdata.[i]) |> Seq.toArray } 

//////////
//===================================================

[<Struct;CustomComparison;CustomEquality>]                              
type WeightPair<'a when 'a: comparison> = 
  val Weight : float; val VertX : 'a ; val VertY:'a      
  new(w:float,x,y) = 
    {Weight = w; VertX = x; VertY = y} 
   
  override x.ToString() = (string x.Weight) + ", " + (x.VertX.ToString()) + "," + (x.VertY.ToString())
 
  interface IEquatable<WeightPair<'a>> with
        member this.Equals(other) =
            this.Weight = other.Weight && this.VertX = other.VertX && this.VertY = other.VertY

  interface IComparable<WeightPair<'a>> with
        member this.CompareTo(other) =
          if this.Weight = other.Weight then
            compare (this.VertX, this.VertY) (other.VertX, other.VertY)
          else this.Weight.CompareTo(other.Weight) 

//===================================================

let rec dispTreeGeneric d maxDepth (fg:FastStringGraph) (visited:Set<string>) dashes spaces node = 
    match (fg.GetEdges node, maxDepth) with 
      | None, _ -> node
      | _, Some n when d >= n -> node
      | Some edges, _ ->        
         let children = 
                edges
                |> Array.filterMap 
                    (visited.Contains >> not) 
                    (fun e -> 
                       spaces + "|" 
                              + (dispTreeGeneric (d+1) maxDepth fg
                                  (visited.Add node) 
                                  (dashes + "-") 
                                  (spaces + "|  ") 
                                  e))

         dashes + node + "\n" + (children |> Strings.joinToStringWith Strings.newLine) 

///renders a graph that is a tree as a string.
let dispStringTree d maxDepth g node = dispTreeGeneric d maxDepth g Set.empty "-" " " node

[<Struct;CustomComparison;CustomEquality>]                              
type WeightPart<'a when 'a: comparison> = 
  val Weight : float; val Vert : 'a       
  new(w:float,x) = 
    {Weight = w; Vert = x} 
   
  override x.ToString() = (string x.Weight) + ", " + (x.Vert.ToString()) 
 
  interface IEquatable<WeightPart<'a>> with
        member this.Equals(other) =
            this.Weight = other.Weight && this.Vert = other.Vert  

  interface IComparable<WeightPart<'a>> with
        member this.CompareTo(other) =
          if this.Weight = other.Weight then
            compare this.Vert other.Vert 
          else this.Weight.CompareTo(other.Weight) 

//===================================================

type WeightedGraph<'a when 'a: equality and 'a:comparison>(?trackweights) = 
    let mutable edges = Dict<'a, Hashset<WeightPart<'a>>>() 
    let mutable edgeWeights = Dict<struct('a * 'a), float>(HashIdentity.Structural)    
    let track_weights = defaultArg trackweights false
    
    member x.EdgeData = edges 

    member x.InsertRange (es,ews) = 
        edges.Clear() 
        edges <- es
        edgeWeights.Clear() 
        edgeWeights <- ews

    member x.InsertVertex(s:'a) =  
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s,Hashset()) 
        contained
    
    member x.Remove(v:'a) = 
       match (edges.tryFind v) with
        | None -> false
        | Some elist ->                                         
           elist |> Seq.iter (fun ((weight_v2)) -> 
                      let vertless,vertHigher = lessToLeft (v,weight_v2.Vert)
                      let node = struct (vertless,vertHigher)
                      match edgeWeights.tryFind node with 
                               | Some _ -> ignore <| edgeWeights.Remove node
                               | _ -> ()
           
                      edges.[weight_v2.Vert].Remove(weight_v2) |> ignore)
           edges.Remove v

           
    member x.InsertEdge (v0,v1, w) = 
        let x,y = lessToLeft (v0,v1)
       
        if not track_weights || not <| edgeWeights.ContainsKey (struct(x,y)) then
          maybe {
          let! elist0 = edges.tryFind v0
          let! elist1 = edges.tryFind v1

          let added1 = elist0.Add (WeightPart(w, v1))
          let added2 = elist1.Add (WeightPart(w, v0))
        
          let _ = if track_weights then ignore <| edgeWeights.ExpandElseAdd (struct(x,y)) id w else ()
          return (added1,added2)} else None

    member x.ContainsVertex v = edges.ContainsKey v 
    
    member x.AdjustWeight f v1 v2 =         
        let vertless,vertHigher = lessToLeft (v1,v2)
        if track_weights then        
          maybe {
          let! w = match edgeWeights.tryFind (struct (vertless,vertHigher)) with 
                     | Some _ as w -> w 
                     | _ -> x.InsertEdge(v1,v2,0.) |> ignore; Some 0.
                 
          let v' = WeightPart(w,v2)
           
          let elist0 = edges.[v1]
          let elist1 = edges.[v2]

          elist0.Remove(v') 

          let w' = f w
          let adj1 = elist0.Add(WeightPart(w',v2))
        
          elist1.Remove(WeightPart(w,v1))
          let adj2 = elist1.Add(WeightPart(w',v1))
        
          edgeWeights.[struct (vertless,vertHigher)] <- w'
        
          return (adj1,adj2)}  
        else None

    member __.EdgeWeights = edgeWeights

    member x.GetEdgeWeight v1 v2 = 
        let a,b = lessToLeft (v1,v2)        
        maybe {
        let! w = edgeWeights.tryFind (struct(a,b))
        return w} 

    member x.ContainsEdge v1 v2 = 
        maybe {
        let! elist0 = edges.tryFind v1 
        return (elist0.Contains v2)}  

    member x.GetEdges v = 
        maybe {
         let! elist = edges.tryFind v  
         return elist }

    member x.Edges = 
      Hashset(
         x.EdgeData
          |> Seq.collect (fun (DictKV(k,v)) -> 
               v |> Seq.map (fun (w_v2) -> lessToLeft(k, w_v2.Vert), w_v2.Weight) 
                 |> Seq.toArray) 
          |> Seq.toArray) |> Seq.toArray

    member x.OrderedEdges =
      let sorted = Collections.Generic.SortedSet()    
      x.EdgeData
      |> Seq.iter 
         (fun (DictKV(k,v)) -> 
           v |> Seq.iter 
                (fun (w_v2) -> 
                  sorted.Add (WeightPart(w_v2.Weight, lessToLeft(k, w_v2.Vert))) 
                  |> ignore  )) 
      sorted

    ///if do max=true it does maximum spanning tree instead
    member x.MinimumSpanningTree(?domax) = 
            let dir = if (defaultArg domax false) then -1. else 1.
            let currentCut = Hashset(edges.Keys)
            let root = currentCut |> Seq.head
            let tree = WeightedGraph()
            let fs = Collections.Generic.SortedSet() 

            let  _, _, steps =
              recurse 
                (fun _ -> currentCut.Count = 0)
                (fun (v, i, getnodes) ->   
                  if getnodes then                            
                    edges.[v] 
                    |> Seq.iter (fun (w_v2) ->                                           
                        if currentCut.Contains w_v2.Vert || currentCut.Contains v then   
                          ignore <| fs.Add (WeightPair(w_v2.Weight * dir,v,w_v2.Vert)) 
                          ())

                  if fs.Count = 0 then (currentCut |> Seq.head, i + 1, true)
                  else 
                    let v0,v2, w, next = 
                        let minel = fs.Min 
                        minel.VertX, minel.VertY, minel.Weight, minel
                          
                    let _ = fs.Remove next     

                    if (currentCut.Contains v0 || currentCut.Contains v2 ) then 
                      let vin0 = tree.InsertVertex v0
                      let vin = tree.InsertVertex v2
                      let nedge = tree.InsertEdge (v0, v2,w)
                          
                      let _ = currentCut.Remove v0 
                      let _ = currentCut.Remove v2 
                      (v2, i + 1, true)
                    else (v, i + 1, false)) (root, 0, true)    
            tree                         
  
    member x.Vertices = Hashset(edges.Keys)  
    
    member g.dijkstra_full source = 
     let dists = Dict.ofSeq [source, 0.] 
     let prev = Dict()
     let vs = g.Vertices
     let q = FibHeap.create ()
     let visited = Hashset()
     let nodeMap = Dict()
                         
     for v in vs do
       if v <> source then dists.Add(v, Double.MaxValue)
       let _ = prev.Add(v, None) 
       let n = FibHeap.insert_data q v dists.[v]
       nodeMap.Add(v, n)
     
     recurse 
      (fun stop -> stop || FibHeap.size q <= 0)
      (fun _ -> 
        let next = FibHeap.extract_min_data q   
         
        let adjs = g.GetEdges next
        let _ = visited.Add next      
                
        match adjs with 
          | None -> false
          | Some vs ->  
            vs |> Seq.iter (fun v2 -> 
              if not (visited.Contains v2.Vert) then
                let alt = dists.[next] + v2.Weight                               
                if alt < dists.[v2.Vert] then 
                  dists.[v2.Vert] <- alt
                  prev.[v2.Vert] <- Some next
                  FibHeap.decrease_key q nodeMap.[v2.Vert] alt)
            false) (false) |> ignore
     dists, prev 
    
    member g.shortest_dists (dists:Dict<_,_>) (prev : Dict<_,_>) target =
        recurse (fst >> Option.isNone)
                (fun (Some p,l) -> prev.getOrDefault None p, (p,dists.[p])::l) 
                (Some target, [])  
    
    member g.shortest_path (prev : Dict<_,_>) target =
        recurse (fst >> Option.isNone)
                (fun (Some p,l) -> prev.getOrDefault None p, p::l) 
                (Some target, [])  
                      
    member g.dijkstra source target = 
     let dists = Dict.ofSeq [source, 0.] 
     let prev = Dict()
     let vs = g.Vertices
     let q = FibHeap.create ()
     let visited = Hashset()
     let nodeMap = Dict()
                         
     for v in vs do
       if v <> source then dists.Add(v, Double.MaxValue)
       let _ = prev.Add(v, None) 
       let n = FibHeap.insert_data q v dists.[v]
       nodeMap.Add(v, n)
     
     recurse 
      (fun stop -> stop || FibHeap.size q <= 0)
      (fun _ -> 
        let next = FibHeap.extract_min_data q   
        if next = target then true
        else 
          let adjs = g.GetEdges next
          let _ = visited.Add next      
                
          match adjs with 
            | None -> false
            | Some vs ->  
              vs |> Seq.iter (fun v2 -> 
                if not (visited.Contains v2.Vert) then
                  let alt = dists.[next] + v2.Weight                               
                  if alt < dists.[v2.Vert] then 
                    dists.[v2.Vert] <- alt
                    prev.[v2.Vert] <- Some next
                    FibHeap.decrease_key q nodeMap.[v2.Vert] alt)
              false) (false) |> ignore  
      
     recurse (fst >> Option.isNone)
             (fun (Some p,l) -> prev.getOrDefault None p, p::l) 
             (Some target, [])  
