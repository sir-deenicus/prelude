open System.Windows.Forms

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



let dispTree = foldTree ("",0) 
                         (fun (s,n) -> s,n+1) 
                         (fun (s,i) n -> s + "\n|" + String.replicate i "_" + n,i) 
                         (fun n l -> l |> List.fold (fun (s1,i1) (s2,i2) -> s1 + s2, max i1 i2) n)

let treeDepth2 t = foldTree 0 ((+) 1) (fun s _ -> s) (fun _ -> List.max) t 

let calendar = new System.Globalization.JulianCalendar();
let today=System.DateTime.Today;
calendar.ToDateTime(today.Year, today.Month, today.Day, today.Hour, today.Minute, today.Second, today.Millisecond);

let toJulian (dateTime:System.DateTime) =
    let day = int64 dateTime.Day
    let _month = int64 dateTime.Month
    let _year = int64 dateTime.Year
 
    let month, year = if _month < 3L then
                          _month + 12L, _year - 1L
                      else _month,_year
 
    day + (153L * month - 457L) / 5L + 365L * year + (year / 4L) - (year / 100L) + (year / 400L) + 1721119L

let toJulian2 (date:System.DateTime) = date.ToOADate() + 2415018.5;
toJulian2 (System.DateTime(1980,1,1)), toJulian2 (System.DateTime(2012,1,1))

let t = Branch("", [Branch("a", [Node "ab";Node "aa"]);Branch("b", [Node "ba";Node "bat"]); Node "d"])

depthFirstInsert ((=) "bat") (Node "bats") t

(Node("a")) |> treeDepth2

treeDepth 0 (Node("a"))

Node("a") |> depthFirstMap (fun x -> x.Length)

Branch("r", [Node("a"); Branch("a", [Node("b")]); Node "c"]) |> depthFirstMap (fun x -> x.Length)

Branch("r", [Node("a"); Branch("a", [Node("b")]); Node "c"]) |> countTreeNodes
Branch("r", [Node("a"); Node("b"); Node "c"]) |> countTreeNodes 

Branch("r", [Node("a"); Branch("a", [Node("b")]); Node "c"]) 

treeDepth 0 <|  Branch("r", [Node("a"); Branch("a", [Node("b")]); Node "c"])

Branch("r", [Node("a"); Branch("a", [Node("b")]); Node "c"]) |> mergeTree

Branch("r", [Node("a"); Branch("a", [Node("b")]); Node "c"]) |> mergeTreeBelowDepth 1 1

Node("a") |> depthFirstFilter false ((=) "c")

Branch("r", [Node("a"); Branch("a", [Node("b")]); Node "c"]) |> depthFirstFilter false ((=) "c")

Branch("r", [Node("a"); Branch("z", [Node("b")]); Node "c"])  |> depthFirstFilter false ((=) "z")

Branch("r", [Node("a"); Branch("a", [Branch("q",[Node("b")])]); Node "c"])  |> depthFirstFilter false ((=) "b")

Branch("r", [Node("a"); Branch("a", [Branch("q",[Node("b")])]); Node "c"])  |> depthFirstFilter false ((=) "q")

Branch("r", [Node("a"); Branch("a", [Branch("q",[Node("b")])]); Node "c"])  |> depthFirstFilter true ((=) "a")

Branch("r", [Node("a"); Branch("a", [Branch("q",[Node("b")])]); Node "c"])  |> depthFirstFilter false ((=) "a")

Branch("r", [Node("a"); 
             Branch("a", 
                [Branch("q",
                  [Node("b")])]); 
             Node "c"])|> mergeTreeBelowDepth 1 1 

Branch("r", [Node("a"); 
             Branch("a", 
                [Branch("q",
                  [Node("b")])]); 
             Node "c"])|> treeDepth2 

Branch("r", [Node("a"); 
             Branch("a", 
                [Branch("q",
                  [Node("b")])]); 
             Node "c"])
             //|> foldTree 0 (fun _ -> 1) (fun s _ -> s) (+)

Branch("r", [Node("a"); 
Branch("a", 
  [Branch("q",
    [Node("b")])]); 
  Node "c"])|> mergeTreeBelowDepth 1 1 |> treeDepth 0

Branch("r", [Node("a"); 
             Branch("a", 
                [Branch("q",
                  [Branch("b", 
                    [Node "e"]); 
                  Node "z"])]); 
             Node "c";
             Branch("q",[Node "zz"])])
             //|> mergeTreeBelowDepth 3 1 
             //|> foldTree 0 (fun s -> 1) (fun s _ -> s) (List.sum)
             |> foldTree ("",0) 
                         (fun (s,n) -> s,n+1) 
                         (fun (s,i) n -> s + "\n|" + String.replicate i "=" + n,i) 
                         (fun n l -> l |> List.fold (fun (s1,i1) (s2,i2) -> s1 + s2, max i1 i2) n)
             

Branch("r", [Node("a"); 
             Branch("a", 
                [Branch("q",
                  [Branch("b", 
                    [Node "e"]); 
                  Node "z"])]); 
             Node "c";
             Branch("q",[Node "zz"])])
             //|> mergeTreeBelowDepth 2 1 
             |> countTreeNodesCollapseBelow 1 1
             //|> countTreeNodes
             //|> treeDepth 0
             //|> dispTree