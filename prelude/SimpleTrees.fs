module SimpleTrees

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
        
 
