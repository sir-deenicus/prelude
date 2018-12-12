module SimpleTrees

open Prelude.Common     
open Prelude.SimpleGraphs
open System

type 'a Tree = 
     | Node of 'a 
     | Branch of 'a * 'a Tree list
     | Empty            


let rec depthFirstMap f = function
      | Node (n) -> Node(f n)
      | Empty -> Empty
      | Branch(n, nodes) ->
            let nodes' = nodes |> List.map (depthFirstMap f)
            Branch(f n, nodes') 
 

let rec depthFirstFilter keepChild f = function
      | Node (n) -> if f n then Node(n) else Empty
      | Empty -> Empty
      | Branch(n, nodes) ->
            if f n then if keepChild then Branch(n,nodes) else Node(n)
            else 
                  let nodes' = nodes |> List.map (depthFirstFilter keepChild f) |> List.filter ((<>) Empty)

                  match nodes' with 
                    |  [] -> Empty
                    | tree -> Branch(n,tree)

let rec mergeTree = function
      | Node (n)  -> [Node n]
      | Empty  -> []
      | Branch(n, nodes) ->
            let nodes' = nodes |> List.collect (mergeTree)
            Node n::nodes'

let rec mergeTreeBelowDepth maxd d = function        
      | Branch(n,bs) when d < maxd ->
          Branch(n, List.map (mergeTreeBelowDepth maxd (d+1)) bs)
      | Node _ as n -> n
      | Empty as n -> n 
      | Branch(_,_) as branch when d>= maxd ->
          match mergeTree branch with           
           | Node n::ns -> Branch(n,ns)
           | [n] -> n
           | _ -> Empty //shouldn't happen      
      | n -> n  
      
let foldTree state modifyState modifyNode foldBranch = 
     let rec foldtree state = function  
               | Branch(n,bs) -> List.map (foldtree (modifyState state)) bs |> foldBranch (modifyNode state n) 
               | Node n -> modifyNode state n
               | Empty -> state
     foldtree state 

let rec treeDepth d = function  
     | Branch(_,bs) ->  List.map (treeDepth (d+1)) bs |> List.max 
     | Node _ -> d
     | Empty -> 0

let rec countTreeNodes = function  
     | Branch(_,bs) ->  
          let tots,brs = List.map (countTreeNodes) bs |> List.unzip
          let sum = List.sum tots 
          printfn "%A" (tots,sum)
          sum+1,Branch(sum, brs)    
     | Node _ -> 1, Node 0
     | Empty -> 0,Empty

let rec countTreeNodesCollapseBelow d n = function 
     | Branch(_,bs) when n > d ->  
          let tots,_ = List.map (countTreeNodesCollapseBelow d (n+1)) bs |> List.unzip
          let sum = List.sum tots 
          sum + 1, Node sum   
      
     | Branch(_,bs) ->  
          let tots,brs = List.map (countTreeNodesCollapseBelow d (n+1)) bs |> List.unzip
          let sum = List.sum tots
          sum+1,Branch(sum, brs)    
     | Node _ -> 1, Node 0
     | Empty -> 0,Empty


let dispTree f = foldTree ("",0) 
                         (fun (s,n) -> s,n+1) 
                         (fun (s,i) n -> s + "\n|" + String.replicate i "_" + f n,i) 
                         (fun n l -> l |> List.fold (fun (s1,i1) (s2,i2) -> s1 + s2, max i1 i2) n)


let rec weightedGraphToTree (fg:WeightedGraph<_>) (visited:Set<string>) (node:WeightedNode<string>) = 
    match (fg.GetEdges node.Node) with 
      | Some h when h.Count = 0 -> Node (node.Node,node.Weight)  
      | None -> Node (node.Node,node.Weight)  
      | Some edges  ->        
         let children = 
                edges
                |> Seq.toArray                  
                |> Array.filterMap 
                    (fun w -> w.Node |> visited.Contains |> not) 
                    (fun e -> 
                        weightedGraphToTree fg (visited.Add node.Node) (e))

         if children.Length = 0 then Node (node.Node,node.Weight)  
         else Branch ((node.Node,node.Weight), List.ofArray children)
         

let rec depthFirstInsert f node = function
      | Node (n) -> if f n then Branch(n,[node]) else Node n
      | Empty -> node
      | Branch(n, nodes) ->
            if f n then Branch(n,node::nodes)
            else 
                  let nodes' = nodes |> List.map (depthFirstInsert f node) |> List.filter ((<>) Empty)

                  match nodes' with 
                    |  [] -> Empty
                    | tree -> Branch(n,tree)

 
let nodeNameStr f = function | Empty -> "" | Node n -> f n | Branch (n,_) -> f n


let rec treeToVerticesAndEdges parentNode = function  
      | Node n -> [n], [(parentNode,n)]
      | Branch(n,bs) -> let g = bs |> List.map (treeToVerticesAndEdges n) 
                        let vs,es = List.unzip g
                        n::List.concat vs,(parentNode,n)::List.concat es      
      | Empty -> [],[]
