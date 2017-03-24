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
        
 

Node("a") |> depthFirstMap (fun x -> x.Length)

Branch("r", [Node("a"); Branch("a", [Node("b")]); Node "c"]) |> depthFirstMap (fun x -> x.Length)

Node("a") |> depthFirstFilter false ((=) "c")

Branch("r", [Node("a"); Branch("a", [Node("b")]); Node "c"]) |> depthFirstFilter false ((=) "c")

Branch("r", [Node("a"); Branch("z", [Node("b")]); Node "c"])  |> depthFirstFilter false ((=) "z")

Branch("r", [Node("a"); Branch("a", [Branch("q",[Node("b")])]); Node "c"])  |> depthFirstFilter false ((=) "b")

Branch("r", [Node("a"); Branch("a", [Branch("q",[Node("b")])]); Node "c"])  |> depthFirstFilter false ((=) "q")

Branch("r", [Node("a"); Branch("a", [Branch("q",[Node("b")])]); Node "c"])  |> depthFirstFilter true ((=) "a")

Branch("r", [Node("a"); Branch("a", [Branch("q",[Node("b")])]); Node "c"])  |> depthFirstFilter false ((=) "a")
 