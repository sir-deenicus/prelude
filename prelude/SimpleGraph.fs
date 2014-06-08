module Prelude.SimpleGraphs

open Prelude.Common
open System

type G<'a,'b>() =
  let x = ()

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

