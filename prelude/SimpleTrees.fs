module Prelude.SimpleTrees

open Prelude.Common
open Prelude.SimpleGraphs
open System

[<RequireQualifiedAccess>]
type SimpleBinaryTree<'a> =
    | Node of 'a
    | Branch of ('a SimpleBinaryTree * 'a SimpleBinaryTree)
      
type Tree<'a> =
    | Node of 'a
    | Branch of 'a * 'a Tree list
    | Empty
     
let nodeName f =
    function
    | Empty -> ""
    | Node n -> f n
    | Branch(n, _) -> f n

let getNode empty =
    function
    | Empty -> empty
    | Node n -> n
    | Branch(n, _) -> n

//====================================

module SimpleBinaryTree =
    let rec flatten = function 
        | SimpleBinaryTree.Node n -> [n]
        | SimpleBinaryTree.Branch(l,r) ->
            let leftlist, rightlist = flatten l, flatten r
            if leftlist.Length < rightlist.Length then leftlist @ rightlist
            else rightlist @ leftlist

    let rec toTree defnode = function 
        | SimpleBinaryTree.Node n -> Node n
        | SimpleBinaryTree.Branch(l,r) -> 
            Branch(defnode(), [toTree defnode l; toTree defnode r])

    let rec toTree2 aggr = function 
        | SimpleBinaryTree.Node n -> Node n
        | SimpleBinaryTree.Branch(l,r) -> 
            let flatl, flatr = flatten l, flatten r
            Branch(aggr(flatl @ flatr), [toTree2 aggr l; toTree2 aggr r])
////////////////////////////////////////////////////////////

let rec depthFirstInsert f node =
    function
    | Node(n) ->
        if f n then Branch(n, [ node ])
        else Node n
    | Empty -> node
    | Branch(n, nodes) ->
        if f n then Branch(n, node :: nodes)
        else
            let nodes' =
                nodes
                |> List.map (depthFirstInsert f node)
                |> List.filter ((<>) Empty)
            match nodes' with
            | [] -> Empty
            | tree -> Branch(n, tree)
            
let rec depthFirstMap f =
    function
    | Node(n) -> Node(f n)
    | Empty -> Empty
    | Branch(n, nodes) ->
        let nodes' = nodes |> List.map (depthFirstMap f)
        Branch(f n, nodes')

let rec depthFirstFilter keepChild f =
    function
    | Node(n) ->
        if f n then Node(n) else Empty
    | Empty -> Empty
    | Branch(n, nodes) ->
        if f n then
            if keepChild then Branch(n, nodes)
            else Node(n)
        else
            let nodes' =
                nodes
                |> List.map (depthFirstFilter keepChild f) 
                |> List.filter ((<>) Empty)
            match nodes' with
            | [] -> Empty
            | tree -> Branch(n, tree)
            
let rec find f =
    function
    | Node(n) ->
        if f n then [n] else []
    | Empty -> []
    | Branch(n, nodes) ->
        if f n then [n]
        else
            let res =
                List.tryPick (fun t ->
                    match find f t with
                    | [] -> None
                    | p -> Some p) nodes
            match res with
            | None -> []
            | Some path -> n::path

//====================================

let rec collapseTree =
    function
    | Node(n) -> [ Node n ]
    | Empty -> []
    | Branch(n, nodes) ->
        let nodes' = nodes |> List.collect (collapseTree)
        Node n :: nodes'

let rec collapseTreeBelowDepth maxd d =
    function
    | Branch(n, bs) when d < maxd ->
        Branch(n, List.map (collapseTreeBelowDepth maxd (d + 1)) bs)
    | Node _ as n -> n
    | Empty as n -> n
    | Branch(_, _) as branch when d >= maxd ->
        match collapseTree branch with
        | Node n :: ns -> Branch(n, ns)
        | [ n ] -> n
        | _ -> Empty //shouldn't happen      
    | n -> n

let foldTree state modifyState modifyNode foldBranch =
    let rec foldtree state =
        function
        | Branch(n, bs) ->
            List.map (foldtree (modifyState state)) bs
            |> foldBranch (modifyNode state n)
        | Node n -> modifyNode state n
        | Empty -> state
    foldtree state

let rec treeDepth d =
    function
    | Branch(_, bs) -> List.map (treeDepth (d + 1)) bs |> List.max
    | Node _ -> d
    | Empty -> 0

let rec countTreeNodes =
    function
    | Branch(_, bs) ->
        let tots, brs = List.map (countTreeNodes) bs |> List.unzip
        let sum = List.sum tots
        sum + 1, Branch(sum, brs)
    | Node _ -> 1, Node 0
    | Empty -> 0, Empty

let rec countTreeNodesAndCollapseBelow depth n =
    function
    | Branch(_, bs) when n > depth ->
        let tots, _ =
            List.map (countTreeNodesAndCollapseBelow depth (n + 1)) bs |> List.unzip
        let sum = List.sum tots
        sum + 1, Node sum
    | Branch(_, bs) ->
        let tots, brs =
            List.map (countTreeNodesAndCollapseBelow depth (n + 1)) bs |> List.unzip
        let sum = List.sum tots
        sum + 1, Branch(sum, brs)
    | Node _ -> 1, Node 0
    | Empty -> 0, Empty

//====================================

let dispTree f =
    foldTree ("", 0) 
        (fun (s, n) -> s, n + 1)
        (fun (s, i) n -> s + "\n|" + String.replicate i "_" + f n, i)
        (fun n l ->
            l |> List.fold (fun (s1, i1) (s2, i2) -> s1 + s2, max i1 i2) n)
                
let graphToTreeWith prjfst getEdges (node : _) =
    let rec loop (visited : Set<_>) node =
        match (getEdges (prjfst node)) with
        | Some es when Array.isEmpty es -> Node node
        | None -> Node node
        | Some edges ->
            let children =
                edges 
                |> Array.filterMap 
                    (prjfst >> visited.Contains >> not)
                    (loop (visited.Add (prjfst node)))
            if children.Length = 0 then Node node
            else Branch(node, List.ofArray children) 
    loop Set.empty node

let weightedGraphToTree (fg : IWeightedGraph<_, _>) node0 = graphToTreeWith fst fg.GetWeightedEdges node0

let graphToTree (fg : IGraph<_>) node0 = graphToTreeWith id fg.GetEdges node0
 
let rec toVerticesAndEdges parentNode =
    function
    | Node n -> [ n ], [ (parentNode, n) ]
    | Branch(n, branches) ->
        let g = List.map (toVerticesAndEdges n) branches
        let ns, es = List.unzip g
        n :: List.concat ns, (parentNode, n) :: List.concat es
    | Empty -> [], []
    
let rec toEdges parentNode =
    function
    | Node n -> [ (parentNode, n) ]
    | Branch(n, branches) ->
        let es = List.map (toEdges n) branches
        (parentNode, n) :: List.concat es
    | Empty -> []

let rec flattenWithShortPathBias =
    function
    | Node n -> [ n ]
    | Branch(n, branches) ->
        let ns = List.map flattenWithShortPathBias branches
        n :: (ns |> List.sortBy List.length |> List.concat)
    | Empty -> []
      
