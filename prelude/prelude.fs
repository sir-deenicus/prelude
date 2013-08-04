// Learn more about F# at http://fsharp.net

module Prelude.Common
 
open System 
open System.Collections.Generic  
open System.Threading.Tasks

///////BASIC MONADS

///A nested if builder is a maybe modified to work with raw conditional tests
type NestedIfBuilder() =
    member this.Bind(x, f) =
       if x then f x
       else None 
    member this.Delay(f) = f()
    member this.Return(x) = Some x
    member this.ReturnFrom(x) = x

let nestif = NestedIfBuilder() 

type MaybeBuilder() =
    member this.Bind(x, f) =
        match x with
        | Some(x) -> f(x)
        | _ -> None
    member this.Delay(f) = f()
    member this.Return(x) = Some x
    member this.ReturnFrom(x) = x

let maybe = MaybeBuilder()

let isDouble s = 
    maybe { 
        let b, v = Double.TryParse(s)
        if b then return v else return! None} 
        
let containsDash (s:string) = maybe {if s.Contains("-") then return s else return! None }

let testDouble s = 
    maybe {
            match containsDash s with
             | Some(s) ->
               let range = s.Split('-')
               let numStr, numStr2 = range.[0], range.[1]
               let! num1 = isDouble numStr
               let! num2 = isDouble numStr2     
               return (num1 + num2)/2.
             | _ -> 
                let! num = isDouble s
                return num    } 
    
let (|Double|String|) s = 
       match testDouble s with
          | Some v -> Double v
          | None -> String s

////////////////////////////////////////

let timeThis iters f = 
   let st = Diagnostics.Stopwatch()
   let x, times = f(), Collections.Generic.List()
   st.Start()
   for i in 0..iters - 1 do
     f() |> ignore
   st.Stop()
   x, st.Elapsed

//////////////////FUNCTIONS//////////

let inline curry2 f a b = f(a,b)

let inline curry3 f a b c = f(a,b,c)

let inline uncurry2 f (a,b) = f a b

let inline uncurry3 f (a,b,c) = f a b c

let inline curryfst f a _ = f a

let inline currysnd f _ b = f b 

let inline flip f a b = f b a 

let inline flipApply  f1 f2 e = f2 (f1 e) 

//no overhead as far as I can see
let inline (</) x f = f x
 
let inline (/>) f y = (fun x -> f x y)

/////SEQUENCES///////////////

let filterMap cond f seqs = seq {for el in seqs do if cond el then yield f el} 

let mapFilter f cond seqs = seq {for el in seqs do let mapped = f el in if cond mapped then yield mapped} 

let splitArrEvenly ways (arr:'a[]) =
    let steps = max (arr.Length / ways) 1
    [| for i in 0..steps..arr.Length - steps -> arr.[i..i + steps - 1]|]

let inline contains c l = Seq.exists (fun item -> item = c) l 

let splitArrayByPercent p (array : 'a []) = 
    let take = int(float(array.Length) * p)
    array.[0..take], array.[take+1..array.Length-1] 

let swapArr i j (arr:'a[]) =
    let k = arr.[i]
    arr.[i] <- arr.[j]
    arr.[j] <- k

///////////TUPLES///////////////

let inline swap (x,y) = (y,x)

let inline fst3 (a,b,c) = a

let fst4 (a,b,c,d) = a

let fst5 (a,b,c,d,e) = a
 
let inline snd3 (_,b,_) = b

let inline snd4 (a,b,_,_) = b

let inline third (a,b,c) = c

let inline third4 (_,_,c,_) = c

let inline fourth (_,_,_,el) = el

let inline fifth (_,_,_,_,el,_,_) =  el

let inline sixth (_,_,_,_,_,el,_) =  el
      
type Array with 
 static member inline unzip5toList (arr : ('a * 'b * 'c * 'd * 'e )  []) = arr |> Array.fold (fun (l1,l2,l3,l4,l5) (d1,d2,d3,d4,d5) -> d1 :: l1, d2 :: l2 , d3::l3, d4 :: l4, d5 :: l5 ) ([],[],[],[],[])
 static member inline unzip7toList (arr : ('a * 'b * 'c * 'd * 'e * 'f * 'g)  []) = arr |> Array.fold (fun (l1,l2,l3,l4,l5,l6,l7) (d1,d2,d3,d4,d5,d6,d7) -> d1 :: l1, d2 :: l2 , d3::l3, d4 :: l4, d5 :: l5 , d6::l6, d7::l7) ([],[],[],[],[],[],[])
 static member inline collapse7 arr = arr |> Array.Parallel.collect (fun  (d1,d2,d3,d4,d5,d6,d7) -> [|d1;d2;d3;d4;d5;d6;d7|]) 
 static member inline collapse5 arr = arr |> Array.Parallel.collect (fun  (d1,d2,d3,d4,d5) -> [|d1;d2;d3;d4;d5|]) 
 
  
//////////////////MAPS/////////////////////////////////

// add to a map item a with f to alter a as key and an alter of vlue with initial i

let keyValueToKey (kv:KeyValuePair<_,_>) = kv.Key
let keyValueToValue (kv:KeyValuePair<_,_>) = kv.Value  
let keyValueToPair (kv:KeyValuePair<_,_>) = kv.Key, kv.Value  

let (|DictKV|) (kv : KeyValuePair<'a,'b>) = kv.Key , kv.Value

//combinators don't make sense for mutable types
type Dictionary<'a,'b> with
  member this.getOrDef key def = if this.ContainsKey(key) then this.[key] else def 
 ///Fold over values in dictionary
  member this.foldv f init = this.Values |> Seq.fold f init
  member this.mapIfExistsAdd key f def =
     if this.ContainsKey(key) then
        this.[key] <- f (this.[key])
     else
       this.Add(key,def)
        
let mapAddGeneric map key f initial = 
    if map |> Map.containsKey key then
        Map.add key (f map.[key]) map
     else Map.add key initial map

let inline mapAdd map key = mapAddGeneric map key ((+) 1.) 1.
 
let inline mapGet map key defaultValue = if Map.containsKey key map then map.[key] else defaultValue

let inline mapGetAdd map key defaultValue =  
   if Map.containsKey key map then map.[key], map else defaultValue, map.Add(key, defaultValue)

module Map =
    let inline sum m = m |> Map.fold (curryfst (+)) 0. 
 
    let inline sumGen f m = m |> Map.fold (fun csum _ x -> f x csum) 0.

/////////////////////ARRAY AND ARRAY 2D useful utilites//////////////

let internal foldRow2D, foldCol2D = 1, 0                     

let inline internal cIndex ind k i (m:'a [,]) = if ind = 1 then m.[k,i] else m.[i,k]

type Array with  
  static member inline countElements array = array |> Array.fold mapAdd Map.empty 
  static member countElementsMapThenFilter f filter array = 
       array |> Array.fold (fun counts item -> 
                            let item' = f item 
                            if filter item' then mapAdd counts item' else counts) Map.empty

  static member countAndMax array =  
     let counts = array |> Array.countElements 
     counts, counts |> (Seq.maxBy keyValueToValue) |> keyValueToPair
      
  static member sub2 start ends (arr:'a []) = arr.[start..ends]

//---------Array 2D----------
type 'a ``[,]`` with
    /// the operator --. also gets the rows in parallel, this does not
    member m.rows index = 
        Array.init (m.GetLength(1)) (fun i -> m.[index, i]) 
    ///the operator |. gets the cols in parallel, this does not
    member m.cols index = 
        Array.init (m.GetLength(0)) (fun i -> m.[i, index])

module Array2D =
  let pmap mapf (array:'a [,]) = 
        let r , c = array.GetLength(0) , array.GetLength(1) 
        let narray = Array2D.create r c (mapf array.[0,0])
        Parallel.For( 0, r, fun i -> for j in 0..c-1 do narray.[i, j] <- mapf array.[i, j]) |> ignore   
        narray 

  let foldAt dimension rowOrCol f seed (m:'a[,]) = 
        let top = m.GetLength(dimension)
        let rec fold state = function
                | i when i = top -> state
                | i -> fold (f state (cIndex dimension rowOrCol i m)) (i + 1)
        fold seed 0   
 
  ///fold at row or column
  let foldGen index f seed (m:'a[,]) =
        let ix , xi = if index = 1 then 0, 1 else 1, 0 // fold by row or fold by column
        let top = m.GetLength(ix)   
        let rec fold state = function | i when i = top -> state 
                                      | i -> fold (m |> foldAt xi i f state) (i+1)
        fold seed 0  
  
  let fold f seed (m:'a[,]) = m |> foldGen 1 f seed

/////////////////////////////STRINGS////////////////////

let inline joinToStringWith sep (s:'a seq) = String.Join(sep, s)

let inline joinToStringWithSpace (s:'a seq) = String.Join(" ", s)

let inline joinToString s = joinToStringWith "" s

let inline charArr (s:string) = s.ToCharArray()

let inline charStr (s:string) = s.ToCharArray()  |> Array.map string   

let inline splitstr (splitby : string[]) (str : string) = str.Split(splitby, StringSplitOptions.RemoveEmptyEntries) 

let splitSentenceRegEx s = System.Text.RegularExpressions.Regex.Split(s, @"(?<=(?<![\d.\d])[\n.?!])")

///splits to words using the following characters: [|"." ; " "; "," ; "?"; ":" ; ";" ; "!" ; "#"; "|";  "\010"; "/"; "\\" ; "\"" ; "'"; "(" ; ")"; "\000"; Environment.NewLine|]
let splitToWords = splitstr [|"." ; " "; "," ; "?"; ":" ; ";" ; "!" ; "#"; "|";  "\010"; "/"; "\\" ; "\"" ; "'"; "(" ; ")"; "\000"; Environment.NewLine|]

let inline splitstrDontRemove (splitby : string[]) (str : string) = str.Split(splitby, StringSplitOptions.None) 

let inline tolower (str:string) = str.ToLower()

let superflouschars = [|' '; ','; '.'; '\"' ; '(' ; ')'; ':'; ';' ; '\''; '”'|]

let inline trim (str: string) = str.Trim()

///trims with passed array, variable superflouschars provides: [|' '; ','; '.'; '\"' ; '(' ; ')'; ':'; ';' ; '\''; '”'|]
let trimWith charsToTrim (s:string) = s.Trim(charsToTrim)

///true when [sub]string is contained in [s]tring
let strcontains (sub:string) (s:string) = s.Contains(trim sub)

let (|StrContains|_|) testString (str : string) = 
   if str.Contains(testString) then Some(true) else None  

let (|StrContainsOneOf|_|) (testStrings : string[]) str = 
     testStrings |> Array.tryFind (strcontains str)  

let (|StrContainsAll|_|) (testStrings : string[]) str  = 
   nestif {return (testStrings |> Array.forall (strcontains str))}

/////////////////COUNTING///////////////////////

type System.Collections.Generic.List<'a> with
   static member Length (glist : Collections.Generic.List<'a>) = glist.Count

module Seq =
  let inline sortByDescending f = Seq.sortBy (fun x -> -1 * int(f x))
 
  let inline counts (v:seq<'a>) =  v |> Seq.fold mapAdd Map.empty

  let mode (v:seq<'a>) = (counts v |> Seq.maxBy(fun x -> x.Value)).Key  

  let modeSeq (v:seq<'a>) = (counts v |> Seq.sortBy (fun x -> x.Value))  
 
////////////////////////MISC////////////////////////

///Similar structure to Unfold but does not generate sequences and meant to be used in place of loops.
let fold stopcondition func seed =
    let rec inner = function
      | state when stopcondition state -> state 
      | state -> inner (func state)
    inner seed    
     
let eightBitsToByte (b:Collections.BitArray) = 
     let a = [|0uy|]
     b.CopyTo(a,0) ; a.[0] 

let toBool = (string >> (<>) "0")

let toUnixTime (dateTime: DateTime) = (dateTime.ToUniversalTime() -  DateTime(1970, 1, 1).ToUniversalTime()).TotalSeconds 

let fromUnixTime timestamp =   DateTime(1970,1,1,0,0,0,0).AddSeconds(timestamp)

let fromUnixTimeMicroSec timestamp = DateTime(1970,1,1,0,0,0,0).AddMilliseconds(timestamp/1000.)

//http://madskristensen.net/post/Generate-unique-strings-and-numbers-in-C.aspx
let generateId () = 
   let n = Guid.NewGuid().ToByteArray() |> Array.fold (fun i b -> int64 ((int b) + 1) * i) 1L 
   String.Format("{0:x}", n - DateTime.Now.Ticks) 

///only slightly slower and much better properties than regular hash
let jenkinsOAThash (key: string) =  
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

//-------------------------

type IO.File with
 static member ReadAllTextOrCreate(fname:string) =
     if IO.File.Exists(fname) then
      IO.File.ReadAllText(fname)
     else
      IO.File.WriteAllText(fname, "") |> ignore
      ""

////////
