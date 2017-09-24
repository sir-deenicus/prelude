module Prelude.SimpleDirectedGraphs

open Prelude.Common
open System
open Prelude.Collections.FibonacciHeap
open Prelude.SimpleGraphs


type WeightedDirectedGraph<'a when 'a: equality and 'a:comparison>(?trackweights) = 
    let mutable edges = Dict<'a, Hashset<WeightPart<'a>>>() 
    let mutable edgeWeights = Dict<struct('a * 'a), float>(HashIdentity.Structural)    
    let track_weights = defaultArg trackweights false
    
    
    member __.Clear() = edges.Clear(); edgeWeights.Clear()
    
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

    member x.InEdges v =
        [|for (KeyValue(struct(a,b),_)) in edgeWeights do if b = v then yield (a,b) |]
    
    member x.Remove(v:'a) = 
       match (edges.tryFind v) with
        | None -> false
        | Some elist ->                                         
           elist |> Seq.iter (fun ((weight_v2)) -> 
                      let vert1,vert2 = (v,weight_v2.Vert)
                      let node = struct (vert1,vert2)
                      match edgeWeights.tryFind node with 
                               | Some _ -> ignore <| edgeWeights.Remove node
                               | _ -> ())

           let removes = ResizeArray()
           for (KeyValue(struct(a,b) as node,w)) in edgeWeights do
             if b = v then
                 edges.[a].Remove(WeightPart(w, b))
                 removes.Add(node)
                 ()
           Seq.iter (fun n -> edgeWeights.Remove n |> ignore) removes
           edges.Remove v

           
    member x.InsertEdge (v0,v1, w) =  
       
        if not track_weights || not <| edgeWeights.ContainsKey (struct(v0,v1)) then
          maybe {
          let! elist0 = edges.tryFind v0 

          let added1 = elist0.Add (WeightPart(w, v1))
        
          let _ = if track_weights then ignore <| edgeWeights.ExpandElseAdd (struct(v0,v1)) id w else ()
          return (added1)} else None

    member x.ContainsVertex v = edges.ContainsKey v 
    
    member x.AdjustWeight f v1 v2 =
        if track_weights then         
          maybe {
            let! w = match edgeWeights.tryFind (struct (v1,v2)) with 
                       | Some _ as w -> w 
                       | _ -> x.InsertEdge(v1,v2,0.) |> ignore; Some 0.
                 
            let v' = WeightPart(w,v2)
           
            let elist0 = edges.[v1]    

            elist0.Remove(v') 

            let w' = f w
            let adj1 = elist0.Add(WeightPart(w',v2))    
        
            edgeWeights.[struct (v1,v2)] <- w'
        
            return (adj1)}  
        else None

    member __.EdgeWeights = edgeWeights

    member x.GetEdgeWeight v1 v2 =       
        maybe {
        let! w = edgeWeights.tryFind (struct(v1,v2))
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
      Hashset(x.EdgeData
              |> Seq.collect (fun (DictKV(k,v)) -> 
                   v |> Seq.map (fun (w_v2) -> (k, w_v2.Vert), w_v2.Weight) 
                     |> Seq.toArray) 
              |> Seq.toArray) 
              |> Seq.toArray

    member x.OrderedEdges =
      let sorted = Collections.Generic.SortedSet()    
      x.EdgeData
      |> Seq.iter (fun (DictKV(k,v)) -> 
           v |> Seq.iter (fun (w_v2) -> 
                  sorted.Add (WeightPart(w_v2.Weight, (k, w_v2.Vert))) 
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


//
///Can have any edge type
type WeightedDirectedGraphCompressed<'a, 'b when 'a: equality and 'a:comparison>() = 
    let mutable edges = Dict<int, Dict<int, 'b>>() 

    let vertices = Dict<'a,int>()
    let rev_vertices = Dict<int,'a>()


    member x.EdgeData = edges 
    member __.GraphData = (edges,keyValueSeqtoPairArray vertices,keyValueSeqtoPairArray rev_vertices)
    member x.InsertRange (es,vs,rvs) = 
            edges.Clear()
            vertices.Clear()
            rev_vertices.Clear()
            
            edges <- es

            Array.iter vertices.Add vs
            Array.iter rev_vertices.Add rvs
    
    member __.Clear () = edges.Clear();vertices.Clear(); rev_vertices.Clear()

    member x.ComputeReverseIndex() =
      rev_vertices.Clear()
      for (KeyValue(k,i2)) in vertices do
          rev_vertices.Add(i2,k) 
    
    member x.InsertVertex(s:'a) =  
        let contained = vertices.ContainsKey s
        if not contained then 
            let count = vertices.Count
            vertices.Add(s,count)
            edges.Add(count,Dict()) 
        contained
    
    member x.Remove(v:'a) = 
       match (vertices.tryFind v) with
        | None -> false
        | Some i ->    
                                           
           for (KeyValue(k,es)) in edges do
                 if es.ContainsKey i then
                   es.Remove i
                   ()
                                                  
           edges.Remove i

           
    member x.InsertEdge (v0,v1, w) =  maybe {
          let! i  = vertices.tryFind v0 
          let! i2 = vertices.tryFind v1
          let es = edges.[i]
          es.ExpandElseAdd i2 (fun _ -> w) w
          return true}
                                      

    member x.ContainsVertex v = vertices.ContainsKey v 
    
    member x.AdjustWeight f v1 v2 =
                
        maybe {           
           let! i  = vertices.tryFind v1
           let es = edges.[i]
           let! i2 = vertices.tryFind v2
           let! old = es.tryFind i2
           es.[i2] <- f old 
           return true}                     

    member x.GetEdgeWeight v1 v2 =       
        maybe {
           let! i = vertices.tryFind v1
           let es = edges.[i]
           let! i2 = vertices.tryFind v2
           let! w = es.tryFind i2 
           return w}  

    member x.ContainsEdge v1 v2 = 
        maybe {
          let! i  = vertices.tryFind v1 
          let! i2 = vertices.tryFind v2   
          return (edges.[i].ContainsKey i2)}  

    member x.GetEdges v = 
        maybe {
         let! i = vertices.tryFind v          
         let es = edges.[i]
         let e = [|for (KeyValue(k,w)) in es -> rev_vertices.[k], w|]
         return e}  

    member x.Edges = 
      Hashset(x.EdgeData
              |> Seq.collect (fun (DictKV(k,v)) -> 
                   v |> Seq.map (fun (KeyValue(k2,w)) -> (rev_vertices.[k],rev_vertices.[k2]),w) 
                     |> Seq.toArray) 
              |> Seq.toArray) 
              |> Seq.toArray  
  
    member x.Vertices = [| for kv in vertices -> kv.Key|]  
   

type WeightedDirectedGraph2<'a, 'b when 'a: equality and 'a:comparison>() = 
    let mutable edges = Dict<'a, Dict<'a, 'b>>() 

    member x.EdgeData = edges 
    member x.InsertRange es = edges.Clear(); edges <- es
    member __.Clear () = edges.Clear() 
    member x.InsertVertex(s:'a) =  
        let contained = edges.ContainsKey s
        if not contained then edges.Add(s,Dict()) 
        contained
    
    member x.Remove(v:'a) = 
       match (edges.tryFind v) with
        | None -> false
        | Some _ ->    
                                           
           for (KeyValue(k,es)) in edges do
                 if es.ContainsKey v then
                   es.Remove v
                   ()
                                                  
           edges.Remove v

           
    member x.InsertEdge (v0,v1, w) =  
          match edges.tryFind v0 with
            | None -> ()
            | Some es ->
              es.ExpandElseAdd v1 (fun _ -> w) w
                                      

    member x.ContainsVertex v = edges.ContainsKey v 
    
    member x.AdjustWeight f v1 v2 =
                
        maybe {
           let! es = edges.tryFind v1
           let! old = es.tryFind v2
           es.[v2] <- f old 
           return true}                     

    member x.GetEdgeWeight v1 v2 =       
        maybe {
           let! es = edges.tryFind v1
           let! w = es.tryFind v2 
           return w}  

    member x.ContainsEdge v1 v2 = 
        maybe {
        let! elist0 = edges.tryFind v1 
        return (elist0.ContainsKey v2)}  

    member x.GetEdges v = 
        maybe {
         let! elist = edges.tryFind v  
         return (keyValueSeqtoPairArray elist)}       

    member x.Edges = 
      Hashset(x.EdgeData
              |> Seq.collect (fun (DictKV(k,v)) -> 
                   v |> Seq.map (fun (KeyValue(k2,w)) -> (k,k2),w) 
                     |> Seq.toArray) 
              |> Seq.toArray) 
              |> Seq.toArray       
  
    member x.Vertices = Hashset(edges.Keys)  