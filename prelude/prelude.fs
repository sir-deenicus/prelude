// Learn more about F# at http://fsharp.net

module Prelude.Common
 
open System 
open System.Collections.Generic  
open System.Threading.Tasks

type MutableList<'a> = System.Collections.Generic.List<'a>
type Hashset<'a> = System.Collections.Generic.HashSet<'a>

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
    maybe {  let b, v = Double.TryParse(s)
             if b then return v else return! None} 

let isInt s = 
    maybe { let b, v = Int32.TryParse(s) in if b then return v else return! None} 
        
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
    
let (|Double|String|Int|) s = 
   match isInt s with
    | Some i -> Int i
    | None ->  
       match testDouble s with
        | Some f -> Double f
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

let inline keepLeft f (x,y) = x , f y

let inline keepRight f (x,y) = f x , y

let inline lessToLeft (a,b) = if a < b then a,b else b,a 
  
//no overhead as far as I can see
let inline (</) x f = f x
 
let inline (/>) f y = (fun x -> f x y) 

/////SEQUENCES///////////////

///Similar structure to Unfold but does not necessarily generate sequences and meant to be used in place of loops.
let recurse stopcondition func seed =
    let rec inner = function
      | state when stopcondition state -> state 
      | state -> inner (func state)
    inner seed    
     
//    seq {for el in seqs do 
//          if !counter < n then  
//           if cond el then incr counter; yield f el}   
let filterMapTrunc n cond f (seqs: 'a seq) =
    let counter = ref 0 
    let en = seqs.GetEnumerator() 
    recurse (fun _ -> not(!counter < n && en.MoveNext()))
            (fun curlist ->  
                 let el = en.Current
                 if cond el then 
                   incr counter
                   f el::curlist else curlist) []  
                   
let mapFilterTrunc n f cond (seqs: 'a seq) =
    let counter = ref 0 
    let en = seqs.GetEnumerator() 
    recurse (fun _ -> not(!counter < n && en.MoveNext()))
            (fun curlist ->  
                 let el = f en.Current
                 if cond el then 
                   incr counter
                   el::curlist else curlist) []     

let filterMap cond f seqs = seq {for el in seqs do if cond el then yield f el} 

let mapFilter f cond seqs = seq {for el in seqs do let mapped = f el in if cond mapped then yield mapped} 

let inline contains c l = Seq.exists (fun item -> item = c) l 

let inline containsOne (matchtries:'a Set) lelist = Seq.exists (fun item -> matchtries.Contains item) lelist

let swapArr i j (arr:'a[]) =
    let k = arr.[i]
    arr.[i] <- arr.[j]
    arr.[j] <- k

///////////TUPLES///////////////

let inline swap (x,y) = (y,x)

let inline fst3 (a,b,c) = a

let inline fst4 (a,b,c,d) = a

let inline fst5 (a,b,c,d,e) = a
 
let inline snd3 (_,b,_) = b

let inline snd4 (a,b,_,_) = b

let inline snd5 (_,b,_,d,_) = b 

let inline third (a,b,c) = c

let inline third4 (_,_,c,_) = c

let inline third5 (_,b,c,d,_) = c

let inline fourth (_,_,_,el) = el

let inline fourth5 (_,_,_,d,_) = d

let inline fifth (_,b,c,d,e) = e

let inline fifth7 (_,_,_,_,el,_,_) =  el

let inline sixth7 (_,_,_,_,_,el,_) =  el

//////////////////MAPS/////////////////////////////////

// add to a map item a with f to alter a as key and an alter of vlue with initial i

let keyValueToKey (kv:KeyValuePair<_,_>) = kv.Key
let keyValueToValue (kv:KeyValuePair<_,_>) = kv.Value  
let keyValueToPair (kv:KeyValuePair<_,_>) = kv.Key, kv.Value  

let (|DictKV|) (kv : KeyValuePair<'a,'b>) = kv.Key , kv.Value

type Dict<'a,'b> = Collections.Generic.Dictionary<'a,'b>
 

//combinators don't make sense for mutable types
module Dictionary = 
  let toDict (d:Collections.Generic.IDictionary<'a,'b>) = Dict d

type Dictionary<'a,'b>  with
 // static member ofSeq values = let d = Dict() in values |> Seq.iter (fun kv -> d.Add(kv)); d
  static member ofSeq values = Dict(values |> dict) 
  member this.getOrDef key def = 
     let found,v = this.TryGetValue key
     if found then v else def 

  member this.tryFind key = 
    let found,v = this.TryGetValue key
    if found then Some v else None  

 ///Fold over values in dictionary
  member this.foldV f init = this.Values |> Seq.fold f init
  member this.ExpandElseAdd key expand def =
     if this.ContainsKey(key) then
        this.[key] <- expand (this.[key])
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

    ///the order of small and big does not affect semantics as long as the expand function is commutative. Instead if you know which map is small then
    ///it is clear that the function will run more efficiently
    let merge combine wrap smallermap biggermap = 
      smallermap |> Map.fold (fun newmap key value -> 
                                   mapAddGeneric newmap key (combine value) (wrap value))
                                   biggermap
 
    let inline sumGen f m = m |> Map.fold (fun csum _ x -> f x csum) 0.

/////////////////////ARRAY AND ARRAY 2D useful utilites//////////////

let internal foldRow2D, foldCol2D = 1, 0                     

let inline internal cIndex ind k i (m:'a [,]) = if ind = 1 then m.[k,i] else m.[i,k]

module List =
  let inline sortByDescending f = List.sortBy (fun x -> -1. * float(f x))

type 'a ``[]`` with
  member inline self.LastElement = self.[self.Length - 1]
  member inline self.nthFromLastElement i = self.[self.Length - i - 1]

type Array with  
  static member inline sortByDescending f = Array.sortBy (fun x -> -1. * float(f x))  
  static member inline subOrMax take (a:'a[]) = a.[0..(min (a.Length-1) take)]
  static member inline lastElement (a:'a []) = a.[a.Length - 1]
  static member inline nthFromLastElement i (a:'a []) = a.[a.Length - i - 1]
  static member inline get2 loc (a:'a[]) = a.[loc] 
  static member inline countElements array = array |> Array.fold mapAdd Map.empty 
  static member countElementsMapThenFilter f filter array = 
       array |> Array.fold (fun counts item -> 
                            let item' = f item 
                            if filter item' then mapAdd counts item' else counts) Map.empty

  static member countAndMax array =  
     let counts = array |> Array.countElements 
     counts, counts |> (Seq.maxBy keyValueToValue) |> keyValueToPair
  static member  mapFilter f cond (seqs:'a[]) = [|for el in seqs do let mapped = f el in if cond mapped then yield mapped|] 
  static member sub2 start ends (arr:'a []) = arr.[start..ends]
  static member filterMap cond f seqs = [|for el in seqs do if cond el then yield f el|]
  static member splitEvenly ways (arr:'a[]) =
                 let steps = max (arr.Length / ways) 1
                 [| for i in 0..steps..arr.Length - steps -> arr.[i..i + steps - 1]|]

  static member splitIntoNPieces n (arr : 'a []) =  
    let steps = n
    let len = arr.Length - 1
    [| for i in 0..steps..arr.Length - 1 -> arr.[i..min (i + steps - 1) len]|]

  static member splitByPercent p (array : 'a []) = 
    let take = int(float(array.Length) * p)
    array.[0..take], array.[take+1..array.Length-1] 

  static member inline unzip5toList (arr : ('a * 'b * 'c * 'd * 'e )  []) = arr |> Array.fold (fun (l1,l2,l3,l4,l5) (d1,d2,d3,d4,d5) -> d1 :: l1, d2 :: l2 , d3::l3, d4 :: l4, d5 :: l5 ) ([],[],[],[],[])
  static member inline unzip7toList (arr : ('a * 'b * 'c * 'd * 'e * 'f * 'g)  []) = arr |> Array.fold (fun (l1,l2,l3,l4,l5,l6,l7) (d1,d2,d3,d4,d5,d6,d7) -> d1 :: l1, d2 :: l2 , d3::l3, d4 :: l4, d5 :: l5 , d6::l6, d7::l7) ([],[],[],[],[],[],[])
  static member inline collapse7 arr = arr |> Array.Parallel.collect (fun  (d1,d2,d3,d4,d5,d6,d7) -> [|d1;d2;d3;d4;d5;d6;d7|]) 
  static member inline collapse5 arr = arr |> Array.Parallel.collect (fun  (d1,d2,d3,d4,d5) -> [|d1;d2;d3;d4;d5|]) 
 
module Tuple =
  let toArray5 (a,b,c,d,e) = [|a;b;c;d;e|]

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
 
  ///fold at row or column
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

let getColJagged col (arr:'a [][]) = [|for row in 0..arr.Length - 1 -> row|] |> Array.Parallel.map (fun row -> arr.[row].[col])

/////////////////////////////STRINGS////////////////////
//These are duplicated because they are typically used many times in an inner loop. Genericity and function overhead
//not worth it for more general code

let removeExtraSpaces (s:string) = 
      let sb = Text.StringBuilder()
      s |> Seq.fold (fun waslastspace curchar -> 
                       if curchar = ' ' && waslastspace = true then true 
                       else sb.Append(curchar) |> ignore
                            curchar = ' ') false |> ignore
      sb.ToString().Trim()

let removeExtraStrings (strToStrip:string) (s:string) = 
      let sb = Text.StringBuilder()
      s |> Seq.fold ( fun waslastX curchar -> 
                       let curstr = string curchar
                       if curstr = strToStrip && waslastX = true then true 
                       else sb.Append(curstr) |> ignore
                            curstr = strToStrip) false |> ignore
      sb.ToString().Trim()

let removeExtraChar (charToStrip:char) (s:string) = 
      let sb = Text.StringBuilder()
      s |> Seq.fold ( fun waslastX curchar ->  
                       if curchar = charToStrip && waslastX = true then true 
                       else sb.Append(curchar) |> ignore
                            curchar = charToStrip) false |> ignore
      sb.ToString().Trim()

let removeExtraChars (charsToStrip:char[]) (s:string) = 
      let sb = Text.StringBuilder()
      s |> Seq.fold ( fun waslastX curchar ->  
                       let skipchar = contains curchar charsToStrip
                       if skipchar && waslastX = true then true 
                       else sb.Append(curchar) |> ignore
                            skipchar) false |> ignore
      sb.ToString().Trim()
//-------------
let newLine = Environment.NewLine

let inline joinToStringWith sep (s:'a seq) = String.Join(sep, s)

let inline joinToStringWithSpace (s:'a seq) = String.Join(" ", s)

let inline joinToString s = joinToStringWith "" s

let capitilizeFirst (str:string) = 
  str.Split (' ') |> Array.map (fun words -> 
                                   words |> String.mapi (fun i ch -> if i = 0 then Char.ToUpper ch else ch)) 
                  |> joinToStringWithSpace

///= [|'.' ; ' '; ',' ; '\t'; '?'; ':' ; ';' ; '!' ; '#'; '|';  '\010'; '/'; '\\' ; '\'' ; '(' ; ')'; '\000'; Environment.NewLine; '—'; '<'; '>';'[';']';'“'|]
let splitChars = [|'.' ; ' '; ',' ; '\t'; '?'; '\"'; ':' ; ';' ; '!' ; '#'; '|';  '\010'; '/'; '\\' ; '\'' ; '(' ; ')'; '\000'; '\n'; '—'; '<'; '>';'[';']';'“'|]

let superflouschars = [|' '; ','; '.'; '\"' ; '?';'!'; ';'; '(' ; ')'; ':'; ';'; '*'; '+'; '-' ; '\''; '”'; '{';'}'; '['; ']'; '\010' ; '\000'|]

let inline trim (str: string) = str.Trim()

///trims with passed array, variable superflouschars provides: ' '; ','; '.'; '\"' ; '(' ; ')'; ':'; ';'; '*'; '+'; '-' ; '\''; '”'; '{';'}'; '['; ']'
let trimWith charsToTrim (s:string) = s.Trim(charsToTrim)

///charArr (s:string) = s.ToCharArray()
let inline charArr (s:string) = s.ToCharArray()

///s.ToCharArray() |> Array.map string 
let inline charStr (s:string) = s.ToCharArray() |> Array.map string   

let inline splitstr (splitby : string[]) (str : string) = str.Split(splitby, StringSplitOptions.RemoveEmptyEntries) 

let inline splitstrWithSpace (str : string) = str.Split([|" "|], StringSplitOptions.RemoveEmptyEntries) 

let inline splitSentenceRegEx s = System.Text.RegularExpressions.Regex.Split(s, @"(?<=(?<![\d.\d])[\n.?!])")

let internal slpitStrs = [|"." ; " "; "," ; "\t"; "?"; ":" ; ";" ; "!" ; "#"; "|";  "\010"; "/"; "\\" ; "\"" ; "(" ; ")"; "\000"; Environment.NewLine; "—"; "<"; ">";"[";"]";"“"|]
 
///splits to words using the following characters: \s \t . , ? : ; ! # | \010 / \ " ( ) \000 \n — [ ] “ > <
let splitToWords = splitstr slpitStrs
///Like regular splitToWords but uses a regex to keep numbers like 5.13, 450,230.12 or 4,323 together. Hence slower. splits to words using the following characters:  \s . , ? : ; ! # | \010 / \ " ( ) \000 \n — [ ] “ > <
let inline splitToWordsRegEx s = Text.RegularExpressions.Regex.Split(s, @"(?<![\d\.\d|\d,\d](?!\s))[\s.,?:;!#\|\010/\\""()\000\n—\[\]“\>\<]") |> Array.filterMap ((<>) "") (trimWith superflouschars)

let inline splitstrDontRemove (splitby : string[]) (str : string) = str.Split(splitby, StringSplitOptions.None) 

let inline tolower (str:string) = str.ToLower()

let inline toupper (str:string) = str.ToUpper()

let inline replace (oldvalue:string) replacement (str:string) = str.Replace(oldvalue,replacement)

///true when [sub]string is contained in [s]tring
let inline strcontains (sub:string) (s:string) = s.Contains(trim sub)

///true when [sub]string is contained in [s]tring
let inline containedinStr (s:string) (sub:string) = s.Contains(trim sub)

let strContainsAll testStrings str = testStrings |> Array.forall (containedinStr str) 

let (|StrContains|_|) testString (str : string) = 
   if str.Contains(testString) then Some(true) else None  

let (|StrContainsOneOf|_|) (testStrings : string[]) str = testStrings |> Array.tryFind (containedinStr str) 
      
let (|StrContainsAll|_|) (testStrings : string[]) str = 
    nestif {let! pass = testStrings |> Array.forall (containedinStr str) in return pass}

let (|StrContainsRemove|_|) t (str : string) = 
   if str.Contains(t) then Some(str.Replace(t,"")) else None 

let inline strContainsOneOf testStrings str = (|StrContainsOneOf|_|) testStrings str |> Option.isSome 

let splitwSpace = splitstr [|" "|] 

module String =
    let DecodeFromUtf8Bytes (utf8Bytes:byte []) =  
        System.Text.Encoding.UTF8.GetString(utf8Bytes,0,utf8Bytes.Length) 

    let DecodeFromUtf8 (utf8String:string) = 
        // copy the string as UTF-8 bytes. 
        let utf8Bytes = [|for c in utf8String -> byte c|]
        System.Text.Encoding.UTF8.GetString(utf8Bytes,0,utf8Bytes.Length) 

    let seperateStringByCaps (s:string) = 
     let outString = Text.StringBuilder()
     for c in s do
       if Char.IsUpper c then outString.Append(' '); outString.Append(c)
       else outString.Append c
     outString.ToString()  
/////////////////MUTABLE STRUCTURES AND COUNTING///////////////////////

type System.Collections.Generic.List<'a> with
   static member Length (glist : Collections.Generic.List<'a>) = glist.Count

module Seq =
  let inline sortByDescending f = Seq.sortBy (fun x -> -1. * float(f x))
 
  let inline counts (v:seq<'a>) =  v |> Seq.fold mapAdd Map.empty

  let mode (v:seq<'a>) = (counts v |> Seq.maxBy(fun x -> x.Value)).Key  

  let modeSeq (v:seq<'a>) = (counts v |> Seq.sortBy (fun x -> x.Value))
  
  let takeOrMax n (seqs:'a seq) =
    let counter = ref 0
    let en = seqs.GetEnumerator()   
    seq {  
        while !counter < n && en.MoveNext() do 
            incr counter
            yield en.Current }      

////////////////////////MISC////////////////////////

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

///really semantic sugar for set.min
let minhash hset = hset |> Set.minElement  

let minhashes maxiter fset = 
 let rec gethashes count minset fullset = 
  match count with
   | i when i >= maxiter  || Set.count fullset = 0 -> minset
   | i -> let minim = minhash fullset
          gethashes (i + 1) (Set.add minim minset) (Set.remove minim fullset) 
 gethashes 0 Set.empty fset

//////////////////////////////////

type IO.File with
 static member ReadAllTextOrCreate(fname:string) =
     if IO.File.Exists(fname) then
      IO.File.ReadAllText(fname)
     else
      IO.File.WriteAllText(fname, "") |> ignore
      ""

////////
