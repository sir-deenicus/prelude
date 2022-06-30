// Learn more about F# at http://fsharp.net

module Prelude.Common

open System
open System.Collections.Generic
open System.Threading.Tasks
open System.Net
open System.Linq
open System.Text

type MutableList<'a> = System.Collections.Generic.List<'a>

type Hashset<'a> = System.Collections.Generic.HashSet<'a>

///////ERROR CONTROL FLOW MONADS 
type MaybeBuilder() =
    member __.Bind(x, f) =
        match x with
        | Some(x) -> f(x)
        | _ -> None
    member __.Delay(f) = f()
    member __.Return(x) = Some x
    member __.ReturnFrom(x) = x

let maybe = MaybeBuilder()

type Either<'a,'b> = This of 'a | That of 'b 

let fallibleComputation f x = try Result.Ok(f x) with e -> Result.Error e

let lazyFallibleComputation (x:Lazy<_>) = try Result.Ok(x.Force()) with e -> Result.Error e

///////////////////
module Single =
    let MinMax = Single.MinValue, Single.MaxValue

module Int32 =
    let MinMax = Int32.MinValue, Int32.MaxValue

module Int16 =
    let MinMax = Int16.MinValue, Int16.MaxValue

let inline isNumber (n:string) = Double.TryParse n |> fst

let inline (|ToFloat|) x = float x

let inline (|ToInt|) x = int x

let inline (|ToString|) x = string x

let inline (|ToStringArray|) d = d |> Array.map string

let (|ToArray|) d = d |> Seq.toArray

let (|ToDateTime|) d = DateTime.Parse d

let (|IsDateTime|IsNumber|JustString|) (d : string) =
    let isn, n = Double.TryParse d
    if isn then IsNumber n
    else
        let p, dt = DateTime.TryParse d
        if p then IsDateTime dt
        else JustString d

let toDouble (s : string) =
    maybe {
        let b, v = Double.TryParse(s)
        if b then return v
        else return! None
    }

let toInt (s : string) =
    maybe {
        let b, v = Int32.TryParse(s)
        if b then return v
        else return! None
    }

let containsDash (s : string) =
    maybe {
        if s.Contains("-") then return s
        else return! None
    }

let isRangedDouble s =
    maybe {
        match containsDash s with
        | Some(s) ->
            let range = s.Split('-')
            let numStr, numStr2 = range.[0], range.[1]
            let! num1 = toDouble numStr
            let! num2 = toDouble numStr2
            return (num1 + num2) / 2.
        | _ -> let! num = toDouble s
               return num
    }

let (|Double|String|Int|) s =
    match toInt s with
    | Some i -> Int i
    | None ->
        match toDouble s with
        | Some f -> Double f
        | None -> String s

let inline round (places:int) (num: float) = Math.Round(num, places)

module Option =
    let getDef def x =
        match x with
        | Some n -> n
        | None -> def

    let liftNull x =
        if isNull x then None
        else Some x

    let mapNull f x =
        if isNull x then None
        else Some(f x)

    let tryWith f =
        function
        | None -> false
        | Some x -> f x

//////////////////FUNCTIONS//////////
 
let inline flip f a b = f b a

let inline keepLeft f (x,y) = x , f y

let inline keepRight f (x,y) = f x , y

let inline lessToLeft (a,b) = if a < b then a,b else b,a

let (>>.) f g x = f x; g x

module Tuple =
    let trimap f (a,b,c) = f a, f b, f c

    let quadmap f (a,b,c,d) = f a, f b, f c, f d

    let pair a b = a,b

module Pair =
    let ofArray (a:_[]) = a.[0], a.[1]

    let toArray (a,b) = [|a;b|]

    let map f (x,y) = (f x, f y)

    let map2 f g (a,b) = f a, g b

    let ofTwoFunctions f g x = f x, g x

    let inline join op (x,y) (u,v) = (op x u, op y v)

    let inline addPairs x y = join (+) x y

    let inline apply op (x,y) = op x y

//no overhead as far as I can see
let inline (</) x f = f x

let inline (/>) f y = fun x -> f x y

let inline konst x _ = x

let inline konst2 _ y = y

let inline fnOr f1 f2 a = f1 a || f2 a

let inline fnAnd f1 f2 a = f1 a && f2 a

let vfst (struct(x, _)) = x

let vsnd (struct(_, y)) = y


///repeated application of f n times
let rec repeat n f x =
    if n = 0 then x else repeat (n-1) f (f x)

    
/////SEQUENCES///////////////

///[Deprecated: do not use] Similar structure to Unfold but does not necessarily generate sequences and meant to be used in place of loops.
let recurse stopcondition func seed =
    let rec inner = function
      | state when stopcondition state -> state
      | state -> inner (func state)
    inner seed

///////////TUPLES///////////////

let inline swap (x,y) = (y,x)

let inline fst3 (a,_,_) = a

let inline fst4 (a,_,_,_) = a

let inline fst5 (a, _, _, _, _) = a

let inline snd3 (_,b,_) = b

let inline snd4 (_,b,_,_) = b

let inline snd5 (_,b,_,_,_) = b

let inline third (_,_,c) = c

let inline third4 (_,_,c,_) = c

let inline third5 (_,_,c,_,_) = c

let inline fourth (_,_,_,el) = el

let inline fourth5 (_,_,_,d,_) = d

let inline fifth (_,_,_,_,e) = e

let inline fifth7 (_,_,_,_,el,_,_) =  el

let inline sixth7 (_,_,_,_,_,el,_) =  el

let fst_fourth (a,_,_,d) = a,d

let fst_third (a,_,c) = a,c

let fst_snd (a,b,_) = a,b

let fst_snd3 (a,b,c) = a,b

let fst_snd5 (a,b,_,_,_) = a,b

let snd_third (_,b,c) = b,c

let third_fourth (_,_,c,d) = c,d

//////////////////MAPS/////////////////////////////////

let keyValueToKey (kv:KeyValuePair<_,_>) = kv.Key
let keyValueToValue (kv:KeyValuePair<_,_>) = kv.Value
let keyValueToPair (kv:KeyValuePair<_,_>) = kv.Key, kv.Value
let keyValueSeqtoPairArray s = [|for KeyValue(k,v) in s -> k, v|]

let (|DictKV|) (kv : KeyValuePair<'a,'b>) = kv.Key , kv.Value

type Dict<'a,'b> = Collections.Generic.Dictionary<'a,'b>

type IDict<'a,'b> = Collections.Generic.IDictionary<'a, 'b>

module IDict =
   let getOrDefault key def (idict:IDict<_,_>) =
     let found,v = idict.TryGetValue key
     if found then v else def

//combinators don't make sense for mutable types
module Dict =
  let ofIDict (d:Collections.Generic.IDictionary<'a,'b>) = Dict d

type Collections.Generic.IDictionary<'a, 'b> with
  member this.tryFindIt key =
    let found,v = this.TryGetValue key
    if found then Some v else None

type Dictionary<'a,'b>  with
 // static member ofSeq values = let d = Dict() in values |> Seq.iter (fun kv -> d.Add(kv)); d
  static member ofSeq values = Dict(values |> dict)

  member this.iter f = for (DictKV(_,v)) in this do f v
  member this.getOrDefault def key =
     let found,v = this.TryGetValue key
     if found then v else def

  member this.tryFind key =
    let found,v = this.TryGetValue key
    if found then Some v else None

 ///Fold over values in dictionary
  member this.foldV f init = this.Values |> Seq.fold f init

  member this.ExpandElseAdd key expand def =
     let found , v = this.TryGetValue key
     if found then
        this.[key] <- expand (v)
     else
       this.Add(key,def)

  member this.GetElseAdd key def =
    let found,v = this.TryGetValue key
    if found then v else this.Add(key,def); def

  member this.foldKV f init = this |> Seq.fold f init

  member this.fold f init =
     let mutable x = init
     for (DictKV(k,v)) in this do
       x <- f x k v
     x


///An inside out memoize function where external sharing of memory helps to easily
///simulate dynamic programming using a cache and recursive calls
///let mem = Dict<_,_>()
///let rec fib = 
///    function
///    | 0 -> 0
///    | 1 -> 1
///    | n -> memoize mem fib (n-1) + memoize mem fib (n-2)
let inline memoize (mem:IDict<_,_>) f x =
    match mem.tryFindIt x with
    | None ->
        let y = f x
        mem.Add(x,y)
        y
    | Some y -> y

let inline memoizeC (mem:Collections.Concurrent.ConcurrentDictionary<_,_>) f x =
    match mem.tryFindIt x with
    | None ->
        let y = f x
        mem.AddOrUpdate(x,y, (fun _ _ -> y)) 
    | Some y -> y

type IDictionary<'k,'v> with
   ///in combine, the first element is the source dictionary's and the other is the other's
   member t.MergeWith combine otherDict =
     for (DictKV(key, item)) in otherDict do
        let isin, thisItem = t.TryGetValue key
        if isin then t.[key] <- combine thisItem item
        else t.Add(key,item)

   member t.MergeNoOverwrites otherDict =
     for (DictKV(key, item)) in otherDict do
        let isin, thisItem = t.TryGetValue key
        if not isin then t.Add(key,item)

module Map =
  let inline sum m = m |> Map.fold (fun sum _ v -> v + sum) Unchecked.defaultof<'a>
  let addOrUpdate map key f initial =
      match Map.tryFind key map with
      | Some item -> Map.add key (f item) map
      | None -> Map.add key initial map

  let inline Incr map key = addOrUpdate map key ((+) 1.) 1.

  let inline defGet map key defaultValue = match Map.tryFind key map with Some item -> item | None -> defaultValue

  let inline getAdd map key defaultValue =
     match Map.tryFind key map with
       | Some item -> item, map
       | None -> defaultValue, map.Add(key, defaultValue)

  let inline normalize (m : Map<'a, 'b>) =
    let total = sum m
    Map.map (fun _ v -> v / total) m

  let flippedGet themap = flip (defGet themap)

  ///the order of small and big does not affect semantics as long as the expand function is commutative. Instead if you know which map is small then
  ///it is clear that the function will run more efficiently
  let merge combine wrap smallermap biggermap =
    smallermap |> Map.fold (fun newmap key value ->
                      addOrUpdate newmap key (combine value) (wrap value))
                      biggermap

  let inline sumGen f m = m |> Map.fold (fun csum _ x -> f x csum) 0.


module DictionarySlim =
    open DictionarySlim

    let ofSeq es =
        let d = DictionarySlim()
        for (k, v) in es do
            let item = &d.GetOrAddValueRef k
            item <- v
        d

/////////////////////ARRAY AND ARRAY 2D useful utilites//////////////

let internal foldRow2D, foldCol2D = 1, 0

let inline internal cIndex ind k i (m:'a [,]) = if ind = 1 then m.[k,i] else m.[i,k]

module List =
    let inline sortByDescending f = List.sortBy (fun x -> -1. * float(f x))
    let inline normalizeWeights (l: ('a * 'b) list) =
        let tot = List.sumBy snd l
        List.map (keepLeft (flip (/) tot)) l
    let mapRight f sq = sq |> List.map (keepLeft f)
    let removeDuplicates (xs:_ list) = List.ofSeq (Hashset(xs))
    let nth i (l:'a list) = l.[i]
    let filterMap filter map xs =
        [ for x in xs do
            if filter x then yield map x ]

    // remove consecutively duplicate entries from a list
    let removeConsecutiveDuplicates items = 
        let rec filter prev xs = [
            match xs with 
            | [] -> ()
            | x::rest -> 
                if Some x = prev then yield! filter prev rest 
                else 
                    yield x
                    yield! filter (Some x) rest
        ]
    
        filter None items 

    
    let cartesianProduct (l1:_ list) (l2: _ list) =
        [ for a in l1 do
            for b in l2 -> a,b]

type 'a ``[]`` with
  member self.LastElement = self.[self.Length - 1]
  member self.nthFromLast(i) = self.[self.Length - i - 1]


module Array =
    let minAndMax typeMinMax (a: _ []) =
        Array.fold (fun (currMin,currMax) x -> min x currMin, max x currMax) (swap typeMinMax) a

    let minAndMaxFloat xs = minAndMax (Single.MinValue, Single.MaxValue) xs 

    let minAndMaxBy f typeMinMax (a: _ []) =
        Array.fold (fun (currMin,currMax) x ->
            let fx = f x
            min fx currMin, max fx currMax) (swap typeMinMax) a

    let minAndMaxFloatBy f xs = minAndMaxBy f (Single.MinValue, Single.MaxValue) xs

    let minAndMaxInt32By f xs = minAndMaxBy f (Int32.MinValue, Int32.MaxValue) xs

    let dropLast n (a : _ []) = a.[..a.Length - 1 - n]

    let removeDuplicates (xs:_ []) = Array.ofSeq (Hashset(xs))

    // remove consecutively duplicate entries from a list 
    let removeConsecutiveDuplicates (items:'a[]) = 
        let rec filter prev i = [|
            if i = items.Length then ()
            else 
                if Some items.[i] = prev then yield! filter prev (i + 1)
                else 
                    yield items.[i]
                    yield! filter (Some items.[i]) (i + 1)
        |]
    
        filter None 0 

    let swapAtIndex i j (arr : 'a []) =
        let k = arr.[i]
        arr.[i] <- arr.[j]
        arr.[j] <- k

    let rot places a =
        let n = Array.length a
        a
        |> Array.permute (fun i ->
               let modulo = ((i + places) % n)
               if i + places < 0 && modulo <> 0 then n + modulo
               else modulo)

    let firstOrDefault def (a : 'a []) =
        if a.Length = 0 then def
        else a.[0]

    let inline lift x = [| x |]

    let pairNexts (a : 'a []) =
        [| for i in 0..2..a.Length - 2 -> a.[i], a.[i + 1] |]

    let inline sortByDescending f = Array.sortBy (fun x -> -1. * float (f x))
    let inline sortByDescendingLinq f (a : 'a []) =
        a.OrderByDescending(System.Func<_, _> f) |> Array.ofSeq
    let inline lastElement (a : 'a []) = a.[a.Length - 1]
    let inline nthFromLastElement i (a : 'a []) = a.[a.Length - i - 1]
    let inline getAt loc (a : 'a []) = a.[loc]
    let inline countElements array = array |> Array.fold Map.Incr Map.empty

    let countElementsMapThenFilter f filter array =
        array
        |> Array.fold (fun counts item ->
               let item' = f item
               if filter item' then Map.Incr counts item'
               else counts) Map.empty

    let countAndMax array =
        let counts = array |> countElements
        counts,
        counts
        |> (Seq.maxBy keyValueToValue)
        |> keyValueToPair

    let takeAtPercent p (a : 'a []) =
        let len = float a.Length in a.[..max 0 (int (round 0 (p * len)) - 1)]

    let mapFilter f cond (seqs : 'a []) =
        [| for el in seqs do
               let mapped = f el in if cond mapped then yield mapped |]

    let subsetStartingAt start (arr : 'a []) = arr.[start..]

    let subsetEndingAt ends (arr : 'a []) = arr.[..ends]

    let subsetSkip (start, skip, ends) (arr : 'a []) =
        [| for i in start..skip..ends -> arr.[i] |]

    let filterMap cond f seqs =
        [| for el in seqs do
               if cond el then yield f el |]

    let splitEvenly ways (arr : 'a []) =
        if arr.Length % ways <> 0 then failwith "Cannot be split evenly"
        let steps = max (arr.Length / ways) 1
        [| for i in 0..steps..arr.Length - steps -> arr.[i..i + steps - 1] |]

    let split_TakeN_atATime n (arr : 'a []) =
        let steps = n
        let len = arr.Length - 1
        [| for i in 0..steps..len -> arr.[i..min (i + steps - 1) len] |]

    let splitByPercent p (array : 'a []) =
        let take = int (float (array.Length) * p)
        array.[0..take], array.[take + 1..array.Length - 1]

    let mapRight f sq = sq |> Array.map (keepLeft f)

    let getColJagged col (arr : 'a [] []) =
        [| for row in 0..arr.Length - 1 -> arr.[row].[col] |]


    ///compresses a 2D array in form [][] into an array of lists with all rows now occupying a single list per column.
    let collapseCols (data : 'a [] []) =
        let outcol = Array.create data.[0].Length []
        for c in 0..data.[0].Length - 1 do
            for r in 0..data.Length - 1 do
                outcol.[c] <- data.[r].[c] :: outcol.[c]
        Array.map List.rev outcol

    let subOrMax take (a : 'a []) = a.[..(min (a.Length - 1) take)]
    let takeOrMax n a = subOrMax n a
    let filterElseTake filter sortfunc min_n n (a : 'a []) =
        let na = Array.filter filter a
        if na.Length < min_n then subOrMax n (sortfunc a)
        else na

    let suffix n (s : 'a []) = s.[max 0 (s.Length - n)..]

    let prefix n (s : 'a []) =
        if s.Length = 0 then s
        else s.[..min (s.Length - 1) n]

    let liftPair (x, y) = [| x; y |]

    let mapiFilter mapf filter (vec : 'a []) =
        let c = ref 0
        [| for a in vec do
               let i = !c
               incr c
               let b = mapf i a
               if filter b then yield b |]

    let filteriMap filter mapf (vec : 'a []) =
        let c = ref 0
        [| for a in vec do
               let i = !c
               incr c
               if filter i a then yield mapf a |]

    let filteri filter (vec : 'a []) =
        let c = ref 0
        [| for a in vec do
               let i = !c
               incr c
               if filter i a then yield a |]

    let oddElements a = filteri (fun i _ -> i % 2 <> 0) a
    let evenElements a = filteri (fun i _ -> i % 2 = 0) a
    let filterToColumn op c (d : 'a [] []) =
        d |> Array.map (filteri (fun i _ -> op c i))
    let filterToColumnSingle op c (d : 'a []) = d |> filteri (fun i _ -> op c i)
    let deleteCol c = filterToColumnSingle (<>) c
    let deleteColumn c = filterToColumn (<>) c
    let selectColumn c = filterToColumn (=) c
    let selectColumns cs = filterToColumn (flip Set.contains) cs
    let deleteColumns cs =
        filterToColumn (fun cols c -> Set.contains c cols |> not) cs
          
    let cartesianProduct (a1:_[]) (a2: _[]) =
        [| for a in a1 do
            for b in a2 -> a,b|]


//---------Array 2D----------
type 'a ``[,]`` with
    member m.RowCount = m.GetLength(0)
    member m.ColumnCount = m.GetLength(1)
    /// the fuction atrow also gets the rows in parallel, this does not
    member m.row index =
        Array.init (m.GetLength(1)) (fun i -> m.[index, i])
    ///the operator atcol gets the cols in parallel, this does not
    member m.col index =
        Array.init (m.GetLength(0)) (fun i -> m.[i, index])

module Array2D =
  let pmap mapf (array:'a [,]) =
        let r , c = array.GetLength(0) , array.GetLength(1)
        let narray = Array2D.create r c (mapf array.[0,0])
        Parallel.For( 0, r, fun i -> for j in 0..c-1 do narray.[i, j] <- mapf array.[i, j]) |> ignore
        narray
  let pmapi mapf (array:'a [,]) =
        let r , c = array.GetLength(0) , array.GetLength(1)
        let narray = Array2D.create r c (mapf 0 0 array.[0,0])
        Parallel.For( 0, r, fun i -> for j in 0..c-1 do narray.[i, j] <- (mapf i j array.[i, j])) |> ignore
        narray

  let foldAt dimension rowOrCol f seed (m:'a[,]) =
        let top = m.GetLength(dimension)
        let rec fold state = function
                | i when i = top -> state
                | i -> fold (f state (cIndex dimension rowOrCol i m)) (i + 1)
        fold seed 0

  ///fold at row or column, where index refers to dimension
  let foldGen index f seed (m:'a[,]) =
        let ix , xi = if index = 1 then 0, 1 else 1, 0 // fold by row or fold by column
        let top = m.GetLength(ix)
        let rec fold state = function | i when i = top -> state
                                      | i -> fold (m |> foldAt xi i f state) (i+1)
        fold seed 0

  let fold f seed (m:'a[,]) = m |> foldGen 1 f seed
 
  ///isParallel
  let atrow (m:'a [,]) index = Array.Parallel.init (m.GetLength(1)) (fun i -> m.[index, i])
  ///isParallel
  let atcol (m:'a [,]) index = Array.Parallel.init (m.GetLength(0)) (fun i -> m.[i, index])

  let toJaggedArray2D (a:_ [,]) = 
      [|for i in 0..a.GetLength(0) - 1 -> 
          [| for j in 0..a.GetLength(1) - 1-> a.[i,j]|]|] 
 
  let map2 f (a:_ [,]) (b: _ [,]) = 
      Array2D.init (a.GetLength(0)) (a.GetLength(1)) (fun i j -> f a.[i,j] b.[i,j])

  let pmap2 f (a : _ [,]) (b : _ [,]) =
      let r, c = a.GetLength(0), a.GetLength(1)
      let narray = Array2D.create r c (f a.[0, 0] b.[0, 0])
      Parallel.For(
          0, r,
          fun i ->
            for j in 0..c - 1 do
               narray.[i, j] <- (f a.[i, j] b.[i, j]))
      |> ignore
      narray
       
/////////////////////////////STRINGS////////////////////

let (|StrContainsRemove|_|) (t : string) (str : string) =
     if str.Contains(t) then Some(str.Replace(t, ""))
     else None

let (|ContainsGroupRegEx|_|) t (str : string) =
     if Text.RegularExpressions.Regex.IsMatch(str, t) then
         let groups = Text.RegularExpressions.Regex.Match(str, t)

         let res =
             [| for g in groups.Groups -> g.Value |]
         if res.Length > 1 then Some(res.[1..])
         else None
     else None

let (|ContainsGroupsRegEx|_|) t (str : string) =
     if Text.RegularExpressions.Regex.IsMatch(str, t) then
         let matches = Text.RegularExpressions.Regex.Matches(str, t)
         [| for m in matches do
                let gs =
                    [| for g in m.Groups -> g.Value |]
                if gs.Length > 1 then yield! gs.[1..] |]
         |> Some
     else None

module String = 
    //These are duplicated because they are typically used many times in an inner loop. Genericity and function overhead
    //not worth it for more general code
    let removeExtraSpaces (s : string) =
        let sb = Text.StringBuilder()
        s
        |> Seq.fold (fun waslastspace curchar ->
               if curchar = ' ' && waslastspace = true then true
               else
                   sb.Append(curchar) |> ignore
                   curchar = ' ') false
        |> ignore
        sb.ToString().Trim()

    let removeExtraChar (charToStrip : char) (s : string) =
        let sb = Text.StringBuilder()
        s
        |> Seq.fold (fun waslastX curchar ->
               if curchar = charToStrip && waslastX = true then true
               else
                   sb.Append(curchar) |> ignore
                   curchar = charToStrip) false
        |> ignore
        sb.ToString().Trim()

    let removeExtraChars (charsToStrip : char Set) (s : string) =
        let sb = Text.StringBuilder()
        s
        |> Seq.fold (fun waslastX curchar ->
               let skipchar = charsToStrip.Contains curchar
               if skipchar && waslastX = true then true
               else
                   sb.Append(curchar) |> ignore
                   skipchar) false
        |> ignore
        sb.ToString().Trim()

    let newLine = Environment.NewLine
    
    let inline joinWith (sep : string) (s : 'a seq) =
        String.Join(sep, s)
        
    let inline joinWithSpace (s : 'a seq) = String.Join(" ", s)
    
    let inline join s = joinWith "" s

    ///=  [|" "; "," ; "\t"; "?"; ":" ;"–"; ";" ; "!" ; "|";  "\010";  "\\" ; "\"" ; "(" ; ")"; "\000"; "\r"; "\n"; Environment.NewLine; "—";"[";"]";"“";"”";"--"|]
    let splitterchars =
        [| ' '; ','; '\t'; '?'; ':'; '–'; ';'; '!'; '|'; '\010'; '\\'; '\"'; '(';
           ')'; '\000'; '\r'; '\n'; '—'; '['; ']'; '“'; '”' |]

    let splitterStrs =
        [| " "; ","; "\t"; "?"; ":"; "–"; ";"; "!"; "|"; "\010"; "\\"; "\""; "(";
           ")"; "\000"; "\r"; "\n"; Environment.NewLine; "—"; "["; "]"; "“"; "”";
           "--" |]

    let inline trim (str : string) = str.Trim()

    ///trims with passed array, variable superflouschars provides: ' '; ','; '.'; '\"' ; '(' ; ')'; ':'; ';'; '*'; '+'; '-' ; '\''; '”'; '{';'}'; '['; ']'
    let trimWith (charsToTrim : _ []) (s : string) = s.Trim(charsToTrim)

    ///charArr (s:string) = s.ToCharArray()
    let inline toCharArray (s : string) = s.ToCharArray() 
    
    let inline split (splitby : string []) (str : string) =
        str.Split(splitby, StringSplitOptions.RemoveEmptyEntries) 
         
    let splitWith s = split [| s |]
    
    let splitKeepEmptyWith (splitters : string) (s : string) =
        s.Split([| splitters |], StringSplitOptions.None)
         

    let inline splitWithSpace (str : string) =
        str.Split([| " " |], StringSplitOptions.RemoveEmptyEntries)
        
    let inline splitSentenceRegEx s =
        System.Text.RegularExpressions.Regex.Split
            (s, @"(?<=(?<![\d.\d])[\n.?!])")  

    ///Note...does not split periods. Splits to words using the following characters: [|" "; "," ; "\t"; "?"; ":" ;"–"; ";" ; "!" ; "|";  "\010";  "\\" ; "\"" ; "(" ; ")"; "\000"; "\r"; "\n"; Environment.NewLine; "—";"[";"]";"“";"”";"--"|]
    let splitToWords = split splitterStrs

    ///Like regular splitToWords but eliminates periods while using a regex to keep numbers like 5.13, 450,230.12 or 4,323 together. Hence slower. splits to words using the following characters:  \s . , ? : ; ! # | \010 / \ " ( ) \000 \n — [ ] “ > <
    let inline splitToWordsRegEx s =
        Text.RegularExpressions.Regex.Split
            (s,
             @"(?<![\d\.\d|\d,\d](?!\s))[\s.,?:;\–!#\|\010/\\""()\000\n—\[\]“\>\<”]")
        |> Array.filterMap ((<>) "") (trimWith splitterchars)

    let inline splitDontRemove (splitby : string []) (str : string) =
        str.Split(splitby, StringSplitOptions.None)
        
    let inline splitRegExKeepSplitters splitters s =
        System.Text.RegularExpressions.Regex.Split
            (s, sprintf @"(?<=[%s])" splitters)
            
    let inline splitregEx splitters s =
        System.Text.RegularExpressions.Regex.Split(s, splitters)
        
    let inline tolower (str : string) = str.ToLower()
    
    let inline toupper (str : string) = str.ToUpper()

    let inline replace (unwantedstr : string) replacement (str : string) =
        str.Replace(unwantedstr, replacement)
         
    let replaceN wordsToReplace replacement str =
        wordsToReplace |> Seq.fold (fun s c -> replace c replacement s) str

    ///true when [sub]string is contained in [s]tring
    let inline contains (sub : string) (s : string) = s.Contains(trim sub)

    ///true when [sub]string is contained in [s]tring
    let inline containsRaw (sub : string) (s : string) = s.Contains(sub)
      
    let containsAll testStrings str =
        testStrings |> Array.forall (fun t -> contains t str)

    let containsNof n (testStrings : string []) (str : string) =
        recurse fst4 (fun (_, _, i, m) ->
            let within = str.Contains testStrings.[i]

            let m' =
                if within then m + 1
                else m

            let satisfied = m' >= n && within
            satisfied || i = testStrings.Length - 1, satisfied, i + 1, m')
            (false, false, 0, 0)
        |> snd4

    let regExReplaceWith replacer pattern str =
        let replace (m : Text.RegularExpressions.Match) =
            match m.Value with
            | ContainsGroupRegEx pattern [| a |] -> replacer a
            | _ -> failwith "Error in string match"
        Text.RegularExpressions.Regex.Replace(str, pattern, replace)

    let inline containsOneOf testStrings str =
        testStrings |> Seq.tryFind (fun t -> contains t str) |> Option.isSome
        
    let inline containsOneOfRaw testStrings str =
        testStrings
        |> Array.tryFind (fun t -> contains t str) 
        |> Option.isSome
         
    let splitSentenceManual (s:string) =
        let sb = Text.StringBuilder()
        let slist = MutableList()
        let strmax = s.Length - 1
        let strmaxless1 = strmax - 1

        for n in 0..strmax do
            let c = s.[n]
            if c = '?' || c = '!'  || c = '\n'
                        || (c = '\r' && n < strmax && s.[n + 1] <> '\n')
                        || (c = '.' && n > 0
                                    && not (Char.IsDigit s.[n - 1])
                                    && not (Char.IsUpper c && s.[n-1] = ' ')
                                    && n > 1
                                    && n < strmax
                                    && not (Char.IsDigit s.[n+1])
                                    && (not (Char.IsLetterOrDigit s.[n+1]) || Char.IsUpper s.[n+1])
                                    && n < strmaxless1 //2 before the end
                                    && (not(Char.IsUpper s.[n-2]) || Char.IsUpper(s.[n+2]))
                                    && (s.[n-2] <> '.' ) //can only look 2 back for a period if next is not capitalized. actually strike that. consider names...hope no one ends sentences with initials
                                    && s.[n+2] <> '.' ) then
                if not(c = '\r' || c = '\n') then sb.Append c |> ignore
                if sb.Length > 0 then slist.Add(sb.ToString())
                let _ = sb.Clear()
                ()
            else sb.Append c |> ignore
        if sb.Length > 0 then slist.Add(sb.ToString())
        slist.ToArray()
        
    let removeSymbols s =
        s
        |> Seq.filter (Char.IsSymbol >> not)
        |> join

    let splitKeepQuotes txt =
        Text.RegularExpressions.Regex.Split(txt, """("[^"]+"|[^"\s]+)""")
        |> Array.filter (trim >> (<>) "")

    let toUTF8Bytes (s : string) =
        let b = System.Text.Encoding.Unicode.GetBytes(s)
        System.Text.Encoding.Convert
            (System.Text.Encoding.Unicode, System.Text.Encoding.UTF8, b)

    let getBytes (s : string) = System.Text.Encoding.Unicode.GetBytes(s)

    ///s.ToCharArray() |> Array.map string
    let inline letters (s : string) = s.ToCharArray() |> Array.map string

    let pad padlen (s : string) =
        s + String.replicate (max 0 (padlen - s.Length)) " "

    let normalizeCapitilization (s : string) =
        s
        |> String.mapi (fun i c ->
               if i = 0 then Char.ToUpper c
               else c)

    let suffix n (s : string) = s.[max 0 (s.Length - n)..]

    ///zlenstr is the string returned if s = ""
    let prefix zlenstr n (s : string) =
        if s.Length = 0 then zlenstr
        else s.[..min (s.Length - 1) n]

    let padcut padlen (s : string) =
        let usestr =
            if s.Length >= padlen then s.[..max 0 (padlen - 3)] + ".."
            else s + "  "
        usestr |> pad padlen

    let reverse (s : string) =
        s |> String.mapi (fun i _ -> s.[s.Length - 1 - i])
        
    let replaceRegEx (patternstring : string) (replacestring : string)
        (inputstring : string) =
        Text.RegularExpressions.Regex.Replace
            (inputstring, patternstring, replacestring)
            
    let replaceRegExIngoresCase (patternstring : string)
        (replacestring : string) (inputstring : string) =
        Text.RegularExpressions.Regex.Replace
            (inputstring, patternstring, replacestring,
             Text.RegularExpressions.RegexOptions.IgnoreCase)

    let replaceTheseGen splitfunc (f : string -> string)
        (items : Hashset<string>) str =
        let spacedstr = str |> splitfunc
        let sbuilder = Text.StringBuilder()
        spacedstr
        |> Array.iter (fun w ->
               if items.Contains w then
                   let _ = sbuilder.Append(f w)
                   sbuilder.Append " " |> ignore
               else
                   sbuilder.Append w |> ignore
                   sbuilder.Append " " |> ignore
               ())
        sbuilder.ToString()

    ///takes a set of words and replaces those words with supplied function
    let replaceThese f = replaceTheseGen splitWithSpace f

    ///convenience function to multiply replace within a string. Uses string.replace
    let replaceMultiple words str =
        words
        |> Seq.fold (fun nstr (w, repw) ->
            if w = "" then nstr
            else nstr |> replace w repw) str

    ///convenience function to multiply modify a string. similar to replace multiple. Goes well with sprintf. Uses string.replace
    let transformMultiple f words str =
        words |> Array.fold (fun nstr w -> nstr |> replace w (f w)) str

    ///convenience function to multiply modify a string. similar to replace multiple. Goes well with sprintf. Uses string.replace
    let transformMultipleNoCase f words str =
        words
        |> Array.fold (fun nstr w -> nstr |> replaceRegExIngoresCase w (f w))
               str

    let uncapitalizeFirstWord =
        String.mapi (fun i c ->
            if i = 0 then Char.ToLower c
            else c)

    let capitalizeFirstWord =
        String.mapi (fun i c ->
            if i = 0 then Char.ToUpper c
            else c)

    let capitilizebySpace minlen s =
        s
        |> splitWithSpace
        |> Array.mapi (fun i s ->
               if i = 0 || s.Length > minlen then s |> capitalizeFirstWord
               else s)
        |> joinWithSpace

    let decodeFromUtf8Bytes(utf8Bytes : byte []) =
        System.Text.Encoding.UTF8.GetString(utf8Bytes, 0, utf8Bytes.Length)
        
    let decodeFromUnicode (stringbytes : byte []) =
        Text.Encoding.Unicode.GetString stringbytes

    let decodeFromUtf8(utf8String : string) =
        // copy the string as UTF-8 bytes.
        let utf8Bytes =
            [| for c in utf8String -> byte c |]
        System.Text.Encoding.UTF8.GetString(utf8Bytes, 0, utf8Bytes.Length)

    let seperateStringByCaps (s : string) =
        let outString = Text.StringBuilder()
        for c in s do
            ignore <| if Char.IsUpper c then
                          ignore <| outString.Append(' ')
                          outString.Append(c)
                      else outString.Append c
        outString.ToString()
        
    ///very simple heuristic
    let splitToParagraphs singlebreak (s : string) =
        if singlebreak then split [| "\n"; "\r\n"; "\r" |] s
        else split [| "\n\n"; "\r\n\r\n"; "\r\r" |] s

    let removeExtrasOfString (strToStrip : string) (s : string) =
        s
        |> splitWith strToStrip
        |> joinWith strToStrip

    ///e.g. ["a";"b";"c"] -> "a,b and c"
    let joinWithThenAnd (punct : string) andor (strings : string seq) =
        let ws = Seq.toArray strings
        match ws.Length with
        | 1 -> ws.[0]
        | 0 -> ""
        | _ ->
            let p1 = String.Join(punct, ws.[..ws.Length - 2])
            p1 + " " + andor + " " + ws.LastElement

type String with 
    member t.Splitby ([<ParamArray>] splitbys : string []) =
        String.split splitbys t
    
    member s.Last = s.[s.Length - 1]
    
    member thisStr.SplitbyKeepEmpty ([<ParamArray>] splitbys : string []) =
        thisStr.Split(splitbys, StringSplitOptions.None)

    member s.GetUTF8Bytes() =
        let b = Encoding.Unicode.GetBytes(s)
        Encoding.Convert
            (System.Text.Encoding.Unicode, System.Text.Encoding.UTF8, b)

    member s.GetBytes() = System.Text.Encoding.Unicode.GetBytes(s)

let (|LowerCase|) s = String.tolower s

let (|UpperCase|) s = String.toupper s

let (|StrContains|_|) (testString : string) (str : string) =
    if str.Contains(testString) then Some(true)
    else None

let (|StrContainsOneOf|_|) (testStrings) str =
    testStrings |> Seq.tryFind (fun t -> String.contains t str)

let (|StrContainsAllRemove|_|) (testStrings : string []) str =
    let pass = testStrings |> Array.forall (fun t -> String.contains t str)
    if pass then Some(testStrings |> Array.fold (fun s t -> String.replace t "" s) str)
    else None
    
let (|StrContainsAll|_|) (testStrings : string []) str =
    let pass = testStrings |> Array.forall (fun t -> String.contains t str)
    if pass then Some true
    else None

/////////////////MUTABLE STRUCTURES AND COUNTING///////////////////////

module Hashset =
  let ofSeq (s:'a seq) = Hashset(s)

type System.Collections.Generic.HashSet<'a> with
    member h.AddRange items =
        for item in items do h.Add item |> ignore

    member h.RemoveRange items =
        for item in items do h.Remove item |> ignore

type System.Collections.Generic.List<'a> with
   static member Length (glist : Collections.Generic.List<'a>) = glist.Count

module Seq =
    let getEnumerator (sq: 'a seq) = sq.GetEnumerator()

    //    seq {for el in seqs do
    //          if !counter < n then
    //           if cond el then incr counter; yield f el}
    ///take first n matches and apply f
    let filterMapTake n cond f (seqs: 'a seq) =
        let counter = ref 0
        let en = seqs.GetEnumerator()
        recurse (fun _ -> not(!counter < n && en.MoveNext()))
                (fun curlist ->
                     let el = en.Current
                     if cond el then
                       incr counter
                       f el::curlist else curlist) []

    ///take first n matches after applying f
    let mapFilterTake n f cond (seqs: 'a seq) =
        let counter = ref 0
        let en = seqs.GetEnumerator()
        recurse (fun _ -> not(!counter < n && en.MoveNext()))
                (fun curlist ->
                     let el = f en.Current
                     if cond el then
                       incr counter
                       el::curlist else curlist) []

    let filterMap cond f seqs =
        seq {
            for el in seqs do
                if cond el then yield f el
        }

    // remove consecutively duplicate entries from a sequence
    let removeConsecutiveDuplicates (items:'a seq) = 
       let mutable prev = None
       let e = items.GetEnumerator()
    
       seq {
           while e.MoveNext() do
               if Some e.Current <> prev then  
                   yield e.Current
                   prev <- Some e.Current
       }

    let mapFilter f cond seqs =
        seq {
            for el in seqs do
                let mapped = f el
                if cond mapped then yield mapped
        }

    let minAndMaxBy f l =
        let smallest = Seq.minBy f l
        let largest = Seq.maxBy f l
        f smallest, f largest

    let map3 f a b c =
        let enuma, enumb, enumc =
            (getEnumerator a, getEnumerator b, getEnumerator c)

        seq { 
            while enuma.MoveNext()
                && enumb.MoveNext()
                && enumc.MoveNext() do
                yield f enuma.Current enumb.Current enumc.Current
        }

    let map4 f a b c d =
        let enuma, enumb, enumc, enumd =
            (getEnumerator a, getEnumerator b, getEnumerator c, getEnumerator d)

        seq { 
            while enuma.MoveNext()
                  && enumb.MoveNext()
                  && enumc.MoveNext()
                  && enumd.MoveNext() do
                yield f enuma.Current enumb.Current enumc.Current enumd.Current
        }

    let zip4 (a: 'a seq) b c d =
        let enuma, enumb, enumc, enumd =
            (getEnumerator a, getEnumerator b, getEnumerator c, getEnumerator d)

        seq { 
            while enuma.MoveNext()
                  && enumb.MoveNext()
                  && enumc.MoveNext()
                  && enumd.MoveNext() do
                yield (enuma.Current, enumb.Current, enumc.Current, enumd.Current)
        }

    let removeDuplicates (xs: 'seq) =
        let memory = Hashset()
        seq {
            for x in xs do
                if not (memory.Contains x) then
                    memory.Add x |> ignore
                    yield x
        }

    let mapGroupByWith f sq = sq |> Seq.map (keepLeft f)

    let inline sortByDescending f = Seq.sortBy (fun x -> -1. * float (f x))

    let inline sortByDescendingLinq f (sequence: 'a seq) =
        sequence.OrderByDescending(System.Func<_, _> f)

    let inline counts (v: seq<'a>) = v |> Seq.fold Map.Incr Map.empty

    let mode (v: seq<'a>) =
        (counts v |> Seq.maxBy (fun x -> x.Value)).Key

    let splitSeqWith2 condition f seqs =
        let first = Seq.head seqs
        let _, current, all =
            seqs
            |> Seq.skip 1
            |> Seq.fold (fun ((prevItem, current, all) as state) item ->
                let item' = f item
                if condition prevItem item' then
                    match current with
                    | [] -> state
                    | cs -> item', [ item ], (List.rev cs) :: all
                else
                    item', item :: current, all) (f first, [first], []) // |> snd |> List.rev
        (List.rev current) :: all |> List.rev

    let splitSeqWith condition f seqs =
        let current, all =
            seqs
            |> Seq.fold (fun ((current, all) as state) item ->
                if condition (f item) then
                    match current with
                    | [] -> state
                    | cs -> [ item ], (List.rev cs) :: all
                else
                    item :: current, all) ([], []) // |> snd |> List.rev

        (List.rev current) :: all |> List.rev

    let filteri filter (vec: 'a seq) =
        let c = ref 0
        seq {
            for a in vec do
                let i = !c
                incr c
                if filter i a then yield a
        }

    let findIndexi cond (s: Collections.Generic.IEnumerable<'a>) =
        let enum = s.GetEnumerator()
        recurse (fst) (fun (_, i) ->
            let x = enum.MoveNext()
            if not x then true, -1
            elif cond i enum.Current then true, i
            else false, i + 1) (false, 0)
        |> snd

    let takeOrMax n (seqs: 'a seq) =
        let counter = ref 0
        let en = seqs.GetEnumerator()
        seq {
            while !counter < n && en.MoveNext() do
                incr counter
                yield en.Current
        }
         
    ///infinite hang risk if one is infinite
    let cartesianProduct (s1: _ seq) (s2: _ seq) =
        seq {
            for a in s1 do for b in s2 -> a, b        
        }

    let (|Singleton|) xs =
         if Seq.isEmpty (Seq.tail xs) then Some (Seq.head xs)
         else None


////////////////////////MISC////////////////////////

type IO.Stream with
  member stream.ReadInt () =
      let sizebuff = Array.create (sizeof<int>) 0uy
      let _ = stream.Read(sizebuff,0,sizebuff.Length)
      BitConverter.ToInt32(sizebuff,0)
  member stream.ReadBool () =
      let sizebuff = Array.create (sizeof<bool>) 0uy
      let _ = stream.Read(sizebuff,0,sizebuff.Length)
      BitConverter.ToBoolean(sizebuff,0)
  member stream.ReadArray size =
    let buffer = Array.create size 0uy
    let _ = stream.Read(buffer, 0,  size)
    buffer

  member stream.WriteArray buffer =
    stream.Write(buffer, 0,  buffer.Length)

  member stream.Reset() =
     stream.Seek(0L,IO.SeekOrigin.Begin)

let inline closeAndDispose (trash: ^a) =
       (^a: (member Close:unit->unit) (trash))
       (^a: (member Dispose:unit->unit) (trash))

let int_to_bytes_BigEnd (i:int) = BitConverter.GetBytes(i) |> Array.rev

let bytes_BigEndToInt (i:byte[]) = BitConverter.ToInt32(i |> Array.rev, 0)

let eightBitsToByte (b:Collections.BitArray) =
     let a = [|0uy|]
     b.CopyTo(a,0) ; a.[0]

let bytesToBoolArray (i:byte []) =
    let b = Collections.BitArray(i)
    let bits = Array.create b.Count false
    b.CopyTo(bits, 0)
    bits

let boolArrayToIntN size zero (b:bool[]) =
    let num = Array.create size zero
    Collections.BitArray(b).CopyTo(num, 0)
    num.[..size - 1]

let int32ToBoolArray (i:int) =
    let b = Collections.BitArray([|i|])
    let bits = Array.create b.Count false
    b.CopyTo(bits, 0)
    bits

let boolArrayToInt32 (b:bool[]) =
    let ints = Array.create 1 0
    Collections.BitArray(b).CopyTo(ints, 0)
    ints.[0]

/////////////////
let MonthsIntToString = Map [1, "January"; 2, "February"; 3, "March"; 4, "April"; 5,"May"; 6, "June"; 7, "July"; 8, "August"; 9, "September"; 10, "October"; 11, "November"; 12, "December"]
////////

let secondsToText = function
    | 0. -> "Now"
    | x when x > 60. && x < 3600. -> let m = (x / 60.) |> round 2 in string m + " minutes"
    | x when x > 3600. && x <= 3600. * 24. -> ((x / 3600.) |> round 1 |> string) + " hours"
    | x when x > 3600. * 24. -> ((x / (3600. * 24.)) |> round 1 |> string) + " days"
    | x -> (x |> round 2 |> string) + " seconds"

let integralAndFractionalPart x = floor x, x - floor x 

let hoursToText =
    let numParts x = floor x, x - floor x

    let partstr (magnHigher:float) magnLower scalesmall (unitHigher:string) unitLower =
        let lowernum = 
            if magnLower = 0. then ""
            else " " + (Math.Round(magnLower * scalesmall).ToString()) + " " + unitLower

        String.Format("{0} {1}{2}", magnHigher.ToString("N0"), unitHigher, lowernum)
        
    function
    | h when h < 0. -> "Already happened"
    | h when h < 1./60. -> Math.Round(h * 3600.).ToString() + " seconds"
    | h when h < 1. -> Math.Round(h * 60.).ToString() + " minutes"
    | h when h < 24. ->
        let hrs,mins = numParts h
        partstr hrs mins 60. "hours" "minutes"
    | h when h > 24. * 7. * 52. ->
        let totyears, months = numParts(h/(24. * 7. * 52.))
        partstr totyears months 12. "years" "months"
    | h when h >= 730. ->
        let totmonths, weeks = numParts(h/730.)
        partstr totmonths weeks 4.345 "months" "weeks"
    | h when h > 24. * 7. ->
        let totweeks, days = numParts (h/(24. * 7.))
        partstr totweeks days 7. "weeks" "days"
    | h  ->
        let totdays, hrs = numParts (h/24.)
        partstr totdays hrs 24. "days" "hours"


type DateTime with
   member dt.StartOfWeek( startOfWeek ) =
        let _diff = dt.DayOfWeek - startOfWeek |> int |> float
        let diff = if _diff < 0. then _diff + 7. else _diff
        dt.AddDays(-1. * diff).Date;

   member dt.StartOfMonth () = dt.AddDays(float -dt.Day + 1.).Date

   member d.ToRoughDateString () =
      let today = DateTime.Now.Date
      let span = (d.Date - today).TotalDays
      let spanh = (d - today).TotalHours

      if span = -1. then d.ToShortTimeString() + ", yesterday"
      elif d.Date = today then d.ToShortTimeString()
      elif span = 1. then d.ToShortTimeString() + ", tomorrow"
      elif span > 1. && d.Day <= 28 then sprintf "the %dth, in %s" d.Day (hoursToText spanh)
      else d.ToShortTimeString() + ", " + d.ToLongDateString()

module DateTime =
    let ToLongDateShortTime (d: DateTime) =
        d.ToLongDateString()
        + ", "
        + d.ToShortTimeString()

    let totalhours (d1: DateTime) (d2: DateTime) = (d2 - d1).TotalHours
    let totaldays (d1: DateTime) (d2: DateTime) = (d2 - d1).TotalDays

    let StartOfWeek (startOfWeek: DayOfWeek) (dt: DateTime) =
        let diff = int (dt.DayOfWeek - startOfWeek)
        let wrap = if diff < 0 then 7 else 0

        dt.AddDays(-1 * (diff + wrap) |> float).Date

    let toUnixTime (dateTime: DateTime) =
        (dateTime.ToUniversalTime() - DateTime(1970, 1, 1))
            .TotalSeconds

    let fromUnixTime timestamp =
        DateTime(1970, 1, 1, 0, 0, 0, 0)
            .AddSeconds(timestamp)

    let inline fromUnixTimeMilli timestamp =
        try
            DateTime(1970, 1, 1, 0, 0, 0, 0)
                .AddSeconds(float timestamp / 1000.)
        with _ -> DateTime.MaxValue

    let fromUnixTimeMicroSec timestamp =
        DateTime(1970, 1, 1, 0, 0, 0, 0)
            .AddMilliseconds(timestamp / 1000.)

let now() = DateTime.Now

let dp str = if str = "" then now() else DateTime.Parse str

//http://madskristensen.net/post/Generate-unique-strings-and-numbers-in-C.aspx
let generateId () =
   let n = Guid.NewGuid().ToByteArray() |> Array.fold (fun i b -> int64 ((int b) + 1) * i) 1L
   String.Format("{0:x}", n - DateTime.Now.Ticks)

let guidString () = Guid.NewGuid().ToString()

let generateStrictId() = sprintf "%s|%A|%A|%A" (generateId()) DateTime.Now.TimeOfDay.TotalSeconds DateTime.Now.DayOfYear DateTime.Now.Year
///only slightly slower and much better properties than regular hash
let jenkinsOAThash (key: string) =
    let keymem = key.ToCharArray()
    let mutable currenthash = 0u
    let len = (key.Length - 1)
    for i in 0..len do
        currenthash <- currenthash + uint32 (keymem.[i])
        currenthash <- currenthash + (currenthash <<< 10)
        currenthash <- currenthash ^^^ (currenthash >>> 6)

    currenthash <- currenthash + (currenthash <<< 3);
    currenthash <- currenthash ^^^ (currenthash >>> 11);
    currenthash <- currenthash + (currenthash <<< 15);
    currenthash

///only slightly slower and much better properties than regular hash
//duplicated to minimize overhead
let inline jenkinsOAThashGeneric (key: ^a []) =
    let mutable currenthash = 0u
    let len = (key.Length - 1)
    for i in 0..len do
        currenthash <- currenthash + uint32 (key.[i])
        currenthash <- currenthash + (currenthash <<< 10)
        currenthash <- currenthash ^^^ (currenthash >>> 6)

    currenthash <- currenthash + (currenthash <<< 3);
    currenthash <- currenthash ^^^ (currenthash >>> 11);
    currenthash <- currenthash + (currenthash <<< 15);
    currenthash

//////////////////////////////////

let DocumentsFolder = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)

let pathCombine path1 path2 = IO.Path.Combine(path1, path2)

let combinePaths = List.fold pathCombine ""

type IO.File with
  ///'appends' to top of file
 static member ReverseAppend (fname:string) encode decoder combine (data:'a) =
      use stream = IO.File.Open(fname, IO.FileMode.Open)
      if stream.Length > int64(Int32.MaxValue) then failwith "File Size too large"
      else
         let data0 = stream.ReadArray (int stream.Length) |> decoder
         let _ = stream.Seek(0L, IO.SeekOrigin.Begin)
         combine(data, data0)
         |> encode
         |> stream.WriteArray

 ///'appends' to top of text file
 static member ReverseAppendUTF (fname:string) (txt:string) = IO.File.ReverseAppend fname String.getBytes String.decodeFromUnicode String.Concat txt

  ///'appends' to top of text file
 static member ReverseAppendUTF8 (fname:string) (txt:string) = IO.File.ReverseAppend fname String.toUTF8Bytes String.decodeFromUtf8Bytes String.Concat txt

 static member ReadAllTextOrCreate(fname:string) =
     if IO.File.Exists(fname) then
      IO.File.ReadAllText(fname)
     else
      IO.File.WriteAllText(fname, "") |> ignore
      ""
 static member ReadAllLinesOrCreate(fname:string) =
     if IO.File.Exists(fname) then
      IO.File.ReadAllLines(fname)
     else
      IO.File.WriteAllLines(fname, [|""|]) |> ignore
      [||]


////////
 

let tohtmlTableString headers d =
    let entryFormat horD xs = xs |> Seq.map (fun x -> sprintf "<t%s>%s</t%s>" horD x horD) |> String.joinWith String.newLine
    let els =
      d
      |> Seq.map (entryFormat "d" >> fun s -> "<tr>" + s + "</tr>")
      |> String.joinWith String.newLine
    sprintf "<table>%s%s</table>" (headers |> entryFormat "h") els

let isUrl urlstr =
      let good, uri = Uri.TryCreate(urlstr, UriKind.Absolute)
      good && (uri.Scheme = Uri.UriSchemeHttp || uri.Scheme = Uri.UriSchemeHttps)

let urlencode str = Uri.EscapeDataString str |> String.replace "%20" "+"

let urldecode str = Uri.UnescapeDataString str |> String.replace "+" " "

let cleanURLofParams str =
  let q = System.Text.RegularExpressions.Regex.Matches (str, "[?|#]")
  if q.Count > 0 then
    let findex = [for l in q -> l.Index] |> List.min
    str.[..findex-1]
  else str

let cleanURLofHash str =
  let q = System.Text.RegularExpressions.Regex.Matches (str, "#")
  if q.Count > 0 then
    let findex = [for l in q -> l.Index] |> List.min
    str.[..findex-1]
  else str

let getUrlPath(url:string) =
  let uloc = url.LastIndexOf '/'
  let q = url.LastIndexOf '?'

  if q <> -1 then url.[..q-1] + "/" else url.[..uloc]

let getUrlRoot(url:string) =
  let uloc = url.IndexOf '/'
  if url.[uloc + 1] = '/' then
    url.[..url.IndexOf ('/',uloc + 2)]
  else url.[..uloc]

///////////////
let getLocalIPs() =
   Dns.GetHostEntry(Dns.GetHostName()).AddressList
      |> Array.filter  (fun ip ->  ip.AddressFamily = Sockets.AddressFamily.InterNetwork)

let getActiveIPsWithNames() =
    let netface = NetworkInformation.NetworkInterface.GetAllNetworkInterfaces()
    netface
        |> Array.collect (fun n ->
                n.GetIPProperties()
                 .UnicastAddresses
                    |> Seq.filterMap
                            (fun ip ->
                                n.OperationalStatus = NetworkInformation.OperationalStatus.Up &&
                                ip.Address.AddressFamily = Sockets.AddressFamily.InterNetwork)
                            (fun ip -> ip.Address , n.Name, n.Description)
                    |> Seq.toArray)

//http://stackoverflow.com/questions/9760468/unshortening-urls
let unshorten (shorturl:string) =
    let req = WebRequest.Create(shorturl) :?> HttpWebRequest
    req.AllowAutoRedirect <- false
    req.GetResponse().Headers.["Location"]


//////////////
//Timing 

type Timed<'T> =
  { Result: 'T 
    ElapsedTotal: TimeSpan 
    ElapsedTimes : TimeSpan list}

//creates timing with first execution skipped
let timeThis n f =
    let sw = Diagnostics.Stopwatch()

    let rec loop c (y, ts) =
        if c = 0 then
            { Result = y
              ElapsedTotal = Seq.reduce (+) ts
              ElapsedTimes = ts }
        else
            sw.Restart()
            let y = f ()
            sw.Stop()
            loop (c - 1) (y, sw.Elapsed :: ts)

    sw.Restart()
    let y = f ()
    sw.Stop()
    loop (n - 1) (y, [])
     
type TimedBuilder(?nIters, ?skipFirst) =
    let skipfirst = defaultArg skipFirst true
    let iters = defaultArg nIters 0
    let sw = Diagnostics.Stopwatch()

    let forloop n body =
        sw.Restart()
        let mutable cstate = body()
        sw.Stop()

        let adjust =
            if skipfirst then 0
            else
                cstate <- { cstate with ElapsedTotal = sw.Elapsed
                                        ElapsedTimes =
                                            sw.Elapsed :: cstate.ElapsedTimes }
                1
        for _ in 1..n - adjust do
            sw.Restart()
            let r = body()
            sw.Stop()
            cstate <- { r with ElapsedTotal = sw.Elapsed + cstate.ElapsedTotal
                               ElapsedTimes = sw.Elapsed :: cstate.ElapsedTimes }
        cstate

    member this.Yield x =
        { Result = x
          ElapsedTotal = TimeSpan.Zero
          ElapsedTimes = [] }

    member this.Return x = this.Yield x

    member this.Delay(f) =
        sw.Restart()
        let r =
            if iters > 0 then forloop iters f
            else f()
        sw.Stop()
        if r.ElapsedTotal = TimeSpan.Zero then
            { r with ElapsedTotal = sw.Elapsed
                     ElapsedTimes = [ sw.Elapsed ] }
        else r

    member this.For(xs : int seq, body) = forloop (Seq.length xs) body

let timed = TimedBuilder()

let timedn n = TimedBuilder(n)

let timeds skipFirst n = TimedBuilder(n, skipFirst)

///////////////////

type ConsoleInterface(fname, onData,onError, ?args, ?onExit) =

   let finfo = Diagnostics.ProcessStartInfo(fname ,
                 RedirectStandardOutput = true ,
                 CreateNoWindow = true         ,
                 RedirectStandardError = true  ,
                 RedirectStandardInput = true  ,
                 UseShellExecute = false       ,
                 WindowStyle = Diagnostics.ProcessWindowStyle.Hidden)

   do match args with | None -> () | Some a -> finfo.Arguments <- a

   let mutable exe = None

   member __.Start(s:string) =
     try
      __.Close()
      exe <- Some <| new Diagnostics.Process(StartInfo = finfo)
      exe.Value.OutputDataReceived.Add onData
      exe.Value.ErrorDataReceived.Add onError
      ignore <| exe.Value.Start()

      match onExit with | None -> () | Some f -> exe.Value.Exited.Add f
      exe.Value.BeginOutputReadLine()
      exe.Value.StandardInput.WriteLine("")
      exe.Value.StandardInput.WriteLine(s)
     with _ -> exe <- None

   member __.Start() = __.Start ""

   member __.SendInput (str:string) =
      match exe with
         | Some exec ->
           if exec.HasExited then __.Close(); __.Start str
           else exec.StandardInput.Flush()
                exec.StandardInput.WriteLine (str)
         | _ -> ()

   member __.Close () =
     match exe with
      | None -> ()
      | Some exec ->
         exec.StandardInput.Close()
         exec.Close()
         exec.Dispose()
         exe <- None

let buildTableRow (collens : _ []) (row : string []) =
    row
    |> Array.mapi (fun i s -> String.pad collens.[i] s)
    |> String.joinWith " | "

let makeTable newline headers title (table: string [] []) =
    let hlen = Array.map String.length headers
    let lens = table |> Array.map (Array.map (String.length))

    let longest =
        [| for c in 0 .. headers.Length - 1 ->
               max hlen.[c]
                   (Array.selectColumn c lens
                    |> Array.map Seq.head
                    |> Array.max) |]

    let t0 =
        table
        |> Array.map (buildTableRow longest)
        |> String.joinWith newline

    let hrow =
        [| headers

           [| for i in 0 .. headers.Length - 1 -> String.replicate longest.[i] "-" |] |]
        |> Array.map (buildTableRow longest)
        |> String.joinWith newline

    String.Format("{0}{1}{1}{2}{1}{3}", (String.toupper title), newline, hrow, t0)