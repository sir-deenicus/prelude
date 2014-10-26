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

//type FastMultiGraph<'a, 'b, 'c when 'a: equality and 'a:comparison>() =  
//    let edges = Dict<'a, Dict<'a, 'c>>() 
//    
//    member x.EdgeData = edges :> Collections.Generic.IDictionary<'a, Dict<'a, 'c>>
//
//    member x.InsertVertex(s:'a,item:'c, update) =  
//        let contained = edges.ContainsKey s
//        if not contained then edges.Add(s, Dict()) 
//        else edges.[s].ExpandElseAdd  update item
//        contained
//
//    member x.InsertEdge (v0,v1, etag) = 
//        maybe {
//        let! elist0 = edges.tryFind v0
//        let! elist1 = edges.tryFind v1
//
//        let in0 = elist0.Add (v1,etag)
//        let in1 = elist1.Add (v0,etag)
//       
//        return (in0,in1)}  
//
//    member x.ContainsVertex v = edges.ContainsKey v 
//
//    member x.ContainsEdge v1 v2 = 
//        maybe {
//        let! elist0 = edges.tryFind v1 
//        return (elist0.ContainsKey v2)}  
//
//    member x.GetEdges v = 
//        maybe {
//        let! elist = edges.tryFind v  
//        return elist |> Seq.toArray } 

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
           elist |> Seq.iter (fun ((weight_v2)) -> edges.[weight_v2.Vert].Remove(weight_v2) |> ignore)
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

    member x.MinimumSpanningTree() = 
            let currentCut = Hashset(edges.Keys)
            let root = currentCut |> Seq.head
            let tree = FastGraphGeneric()
            let fs = Collections.Generic.SortedSet() 

            let  _, _, steps =
              recurse 
                (fun _ -> currentCut.Count = 0)
                (fun (v, i, getnodes) ->   
                  if getnodes then                            
                    edges.[v] 
                    |> Seq.iter (fun (w_v2) ->                                           
                          if currentCut.Contains w_v2.Vert || currentCut.Contains v then   
                            fs.Add (WeightPair(w_v2.Weight,v,w_v2.Vert)) 
                            ())

                  if fs.Count = 0 then (currentCut |> Seq.head, i + 1, true)
                  else 
                    let v0,v2, _, next = 
                        let minel = fs.Min 
                        minel.VertX, minel.VertY, minel.Weight, minel
                          
                    fs.Remove next     

                    if (currentCut.Contains v0 || currentCut.Contains v2 ) then 
                      let vin0 = tree.InsertVertex v0
                      let vin = tree.InsertVertex v2
                      let nedge = tree.InsertEdge (v0, v2)
                          
                      currentCut.Remove v0 
                      currentCut.Remove v2 
                      (v2, i + 1, true)
                    else (v, i + 1, false)) (root, 0, true)    
            tree                         
  
    member x.Vertices = Hashset(edges.Keys)  

    member g.dijkstra source target = 
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
        if next = target then true
        else 
          let adjs = g.GetEdges next
          visited.Add next      
                
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
             (fun (Some p,l) -> prev.getOrDef p None, p::l) 
             (Some target, [])  
