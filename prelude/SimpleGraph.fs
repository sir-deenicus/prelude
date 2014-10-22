module Prelude.SimpleGraphs

open Prelude.Common
open System
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
        | None -> false
        | Some elist ->                                              
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
            let tree = FastGraphGeneric()
        
            let weightDict = edges |> Seq.map (fun (KeyValue(k,sq)) -> k, Collections.Generic.SortedSet(sq)) 
                                   |> Dict.ofSeq 

            recurse (fst3 >> Set.isEmpty)
                    (fun (currentCut,v, i) -> 
                          let elist = weightDict.[v]   
                          match (elist.Count = 0) with 
                           | true -> tree.InsertVertex v |> ignore //if v is root and not connected to anything  
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

                              weightDict.[v2].Remove(wvec) // remove the popped
                              (currentCut.Remove v, v2, i + 1)) (vertices,root, 0) |> ignore
                    
            tree

