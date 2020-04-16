#time "on"

module PairingHeap =
    type PairingTree<'a> =
        | Empty
        | Elem of Top : 'a * SubHeaps : list<PairingTree<'a>>
     
    let find_min = function 
        | Elem (elem, _) -> Some elem
        | Empty -> None

    let rec contains x =
        function
        | Elem(elem, _) when x = elem -> true
        | Elem(_, rest) ->
            let candidates =
                List.filter (find_min
                             >> Option.map (fun y -> y <= x)
                             >> Option.defaultValue false) rest
            List.exists (contains x) candidates
        | Empty -> false 
       
    let meld h1 h2 = 
        match (h1, h2) with 
        | Empty, _ -> h2
        | _ , Empty -> h1
        | Elem (elem, subheaps1), (Elem (elem2, _) as t2) when elem < elem2 ->
            Elem (elem, t2::subheaps1)
        | (Elem _ as t1), Elem (elem2, subheaps2) ->
            Elem (elem2, t1::subheaps2)

    let insert t elem = meld (Elem (Top = elem, SubHeaps = [])) t

    //let rec merge_pairs = function
    //    | [] -> Empty
    //    | [x] -> x
    //    | t1::t2::rest -> meld (meld t1 t2) (merge_pairs rest)
    let merge_pairs t =
        let rec merge_loop cont = function
            | [] -> cont Empty
            | [x] -> cont x
            | t1::t2::rest ->
                merge_loop (fun t -> cont(meld (meld t1 t2) t)) rest
        merge_loop id t  
        
    let delete_min = function
        | Empty -> Empty
        | (Elem (_, sh)) -> merge_pairs sh
         

let x1 = PairingHeap.insert PairingHeap.Empty (0,"x")
let x2 = PairingHeap.insert x1 (3, "b")
let x3 = PairingHeap.insert x2 (-1, "b")

PairingHeap.find_min x3

open PairingHeap

let rec tryUntil curr f =
    function
    | [] -> []
    | t :: ts ->
        let t', r = f t
        if r then (t' :: curr) @ ts
        else tryUntil (t :: curr) f ts

let rec delete test = function  
    | Elem (elem,  rest) as t -> 
         if test elem then delete_min t
         else insert (merge_pairs (List.map (delete test) rest)) elem
    | Empty -> Empty

let delete2 test x = 
    let rec loop = function 
        | Elem (elem, _) as t when test elem -> delete_min t, true
        | Elem (elem, rest) -> insert (merge_pairs (tryUntil [] loop rest)) elem, false 
        | Empty -> Empty, false
    loop x |> fst

let rec update2 test f t = 
    let rec loop = function 
        | Elem (elem, _) as t when test elem -> 
            let t' = delete_min t
            insert t' (f elem) , true
        | Elem (elem,  rest) -> 
              insert (merge_pairs (tryUntil [] loop rest)) elem, false 
        | Empty -> Empty, false
    fst (loop t)

let rec delete3 compare test = function 
    | Elem (elem, _) as t when test elem -> delete_min t
    | Elem (elem,  rest) -> 
        let cand, larger = List.partition (find_min >> Option.map compare >> Option.defaultValue false) rest
        let del = merge_pairs (List.map (delete3 compare test) cand) 
        insert (meld del (merge_pairs larger)) elem
    | Empty -> Empty

let delete3b compare test t = 
    let rec loop =
        function 
        | Elem (elem, _) as t when test elem -> delete_min t, true
        | Elem (elem,  rest) -> 
            let cand, larger = List.partition (find_min >> Option.map compare >> Option.defaultValue false) rest
            let del = merge_pairs (tryUntil [] (loop) cand) 
            insert (meld del (merge_pairs larger)) elem, false
        | Empty -> Empty, false
    fst(loop t)

x3 |> PairingHeap.contains (3, "c")
x3 |> PairingHeap.contains (3, "b")

let zz = [1..500000] |> List.fold PairingHeap.insert PairingHeap.Empty
  
 
let zz2 = insert zz 300

zz |> delete_min


delete ((=) 5) zz   //|> PairingHeap.contains 500
zz2
let yy = update2 ((=) 5) ((+) 30) zz2  //|> merge_pairs
delete2 ((=) 5) zz
delete3 (fun y -> y <= 5) ((=) 5) zz2

update2 ((=) 5) (fun x -> x - 199) zz 
update2 ((=) 5) ((+) 100) zz |> delete_min |> delete_min|> delete_min  |> delete_min     // |> find_min
 
delete ((=) 5) zz   //|> PairingHeap.contains 500

zz |> delete3 (fun y -> y <= 5) ((=) 5)
yy |> find_min
yy |> PairingHeap.contains 7
yy |> delete_min 

 
zz |> PairingHeap.delete_min
zz |> PairingHeap.contains 6
zz |> PairingHeap.delete_min |> PairingHeap.contains 6
x3 |> PairingHeap.delete_min 
x1 |> PairingHeap.delete_min |> PairingHeap.delete_min
