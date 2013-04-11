// Learn more about F# at http://fsharp.net

module Prelude.Common
 
open System 
open System.Collections.Generic  
open System.Threading.Tasks

//////////////////FUNCTIONS//////////

let inline curry2 f a b = f(a,b)

let inline curry3 f a b c = f(a,b,c)

let inline uncurry2 f (a,b) = f a b

let inline uncurry3 f (a,b,c) = f a b c

let inline curryfst f a _ = f a

let inline currysnd f _ b = f b 

let inline flip f a b = f b a 

let inline flipApply  f1 f2 e = f2 (f1 e) 

let inline (</) x f = f x
 
let (/>) f y = (fun x -> f x y)

/////SEQUENCES///////////////

let filterMap f cond seqs = seq {for el in seqs do if cond el then yield f el} 

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

let inline snd3 (_,b,_) = b

let inline snd4 (a,b,_,_) = b

let inline third (a,b,c) = c

let inline third4 (_,_,c,_) = c

let inline fourth (_,_,_,el) = el

let inline fifth (_,_,_,_,el,_,_) =  el

let inline sixth (_,_,_,_,_,el,_) =  el

type Array with
 member inline t.unzip5 (arr : ('a * 'b * 'c * 'd * 'e )  []) = arr |> Array.fold (fun (l1,l2,l3,l4,l5) (d1,d2,d3,d4,d5) -> d1 :: l1, d2 :: l2 , d3::l3, d4 :: l4, d5 :: l5 ) ([],[],[],[],[])
 member inline t.unzip7 (arr : ('a * 'b * 'c * 'd * 'e * 'f * 'g)  []) = arr |> Array.fold (fun (l1,l2,l3,l4,l5,l6,l7) (d1,d2,d3,d4,d5,d6,d7) -> d1 :: l1, d2 :: l2 , d3::l3, d4 :: l4, d5 :: l5 , d6::l6, d7::l7) ([],[],[],[],[],[],[])
 member inline t.collapse7 arr = arr |> Array.Parallel.collect (fun  (d1,d2,d3,d4,d5,d6,d7) -> [|d1;d2;d3;d4;d5;d6;d7|]) 
 member inline t.collapse5 arr = arr |> Array.Parallel.collect (fun  (d1,d2,d3,d4,d5) -> [|d1;d2;d3;d4;d5|]) 

//////////////////MAPS/////////////////////////////////

// add to a map item a with f to alter a as key and an alter of vlue with initial i

let keyValueToKey (kv:KeyValuePair<_,_>) = kv.Key
let keyValueToValue (kv:KeyValuePair<_,_>) = kv.Value  
let keyValueToPair (kv:KeyValuePair<_,_>) = kv.Key, kv.Value  

let mapAddGeneric map key f initial = 
    if map |> Map.containsKey key then
        Map.add key (f map.[key]) map
     else Map.add key initial map

let inline mapAdd map key = mapAddGeneric map key ((+) 1.) 1.
 
let inline mapGet map key defaultValue = if Map.containsKey key map then map.[key] else defaultValue

let inline mapGetAdd map key defaultValue =  
   if Map.containsKey key map then map.[key], map else defaultValue, map.Add(key, defaultValue)

let countArray array = array |> Array.fold mapAdd Map.empty

let countArrayMapFilter f filter array = array |> Array.fold (fun counts item -> 
                                                                let item' = f item 
                                                                if filter item' then mapAdd counts item' else counts) Map.empty

let countMax array =  array |> countArray |> (Seq.maxBy keyValueToValue) |> keyValueToPair

/////////////////////////////STRINGS////////////////////

let inline charArr (s:string) = s.ToCharArray()

let inline charStr (s:string) = s.ToCharArray()  |> Array.map string   

let inline splitstr (splitby : string[]) (str : string) = str.Split(splitby, StringSplitOptions.RemoveEmptyEntries) 

let inline splitstrDontRemove (splitby : string[]) (str : string) = str.Split(splitby, StringSplitOptions.None) 

let inline tolower (str:string) = str.ToLower()

let inline trim (str: string) = str.Trim()

/////////////////COUNTING///////////////////////
 
let inline counts (v:seq<'a>) =  v |> Seq.fold mapAdd Map.empty

let mode (v:seq<'a>) = (counts v |> Seq.maxBy(fun x -> x.Value)).Key  

let modeRaw (v:seq<'a>) = (counts v |> Seq.sortBy (fun x -> x.Value))  
 
let inline sumMap m = m |> Map.fold (curryfst (+)) 0. 
 
let inline sumMapGen f m = m |> Map.fold (curryfst ((+) >> f)) 0. 

//////////////////////////////////////////2D ARRAYS/////////

let foldRow2D, foldCol2D = 1, 0                     

let inline (@@) (m: 'a [,]) index = Array.Parallel.init (m.GetLength(1)) (fun i -> m.[index, i])
let inline (@.) (m: 'a [,]) index = Array.Parallel.init (m.GetLength(0)) (fun i -> m.[i, index])

let inline cIndex ind k i (m:'a [,]) = if ind = 1 then m.[k,i] else m.[i,k]

let arr2DFoldAt r k (m : 'a [,]) f seed = 
        let top = m.GetLength(r)
        let rec fold state = function
                | i when i = top -> state
                | i -> fold (f state (cIndex r k i m)) (i + 1)
        fold seed 0   

let arr2DFoldGen index (m : 'a [,]) f seed =
        let ix , xi = if index = 1 then 0, 1 else 1, 0 // fold by row or fold by column
        let top = m.GetLength(ix)   
        let rec fold state = function | i when i = top -> state 
                                      | i -> fold (arr2DFoldAt xi i m f state) (i+1)
        fold seed 0 

let arr2DFold (m : 'a [,]) f seed = arr2DFoldGen 1 m f seed

let arr2DFoldCol (m : 'a [,]) f seed = arr2DFoldGen 0 m f seed   

let pmap2D mapf (array:'a [,]) = 
        let r , c = array.GetLength(0) , array.GetLength(1) 
        let narray = Array2D.create r c (mapf array.[0,0])
        Parallel.For( 0, r, fun i -> for j in 0..c-1 do narray.[i, j] <- mapf array.[i, j]) |> ignore   
        narray   

//type Array with 
//   static member pfold func (seed:'b) (array:'a[]) =
//         let grown = ref seed         
//         Parallel.ForEach(array, (fun it -> lock grown (fun () -> grown := (func !grown it)))) |> ignore
//         !grown
       
////////////////////////////////////////////////

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

///Similar structure to Unfold but does not generate sequences and meant to be used in place of loops.
let fold cond func seed =
    let rec inner = function
      | state when cond state -> state 
      | state -> inner (func state)
    inner seed    

let bitsToByte (b:Collections.BitArray) = 
     let a = [|0uy|] in b.CopyTo(a,0) ; a.[0] 

let toBool = (string >> (<>) "0")

let toUnixTime (dateTime: DateTime) = (dateTime.ToUniversalTime() -  DateTime(1970, 1, 1).ToUniversalTime()).TotalSeconds 

let fromUnixTime timestamp =   DateTime(1970,1,1,0,0,0,0).AddSeconds(timestamp).ToUniversalTime()

//http://madskristensen.net/post/Generate-unique-strings-and-numbers-in-C.aspx
let generateId () = 
   let n = Guid.NewGuid().ToByteArray() |> Array.fold (fun i b -> int64 ((int b) + 1) * i) 1L 
   String.Format("{0:x}", n - DateTime.Now.Ticks) 
 


