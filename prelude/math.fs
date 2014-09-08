module Prelude.Math

open System.Threading.Tasks
open Prelude.Common
open System

let random = new Random()  

let pi = Math.PI  

let inline pow x y = Math.Pow(float x,float y)

let inline squared (x: ^a) = x * x  

let log2 = log >> (flip (/) (log 2.))

let nearestPow2 x = let n = log2 x |> ceil in  2. ** n

let inline isPowerOf2 (ToFloat x) = let y = log2 x in y - floor y = 0. 

let inline addInPlaceIntoFirst (a1:'a []) (a2:'a[]) =
    for i in 0..a1.Length - 1 do 
       a1.[i] <- a1.[i] + a2.[i]

//***************************BASIC STATS******************//

let cdf p = p |> Array.fold (fun (total, list) v -> 
                                let cd = v + total
                                cd, cd::list ) (0., []) 
               |> snd 
               |> Array.ofList 

let getDiscreteSample (pcdf:float[]) = 
    let k, pcdlen = random.NextDouble() * pcdf.[0], pcdf.Length - 1
    let rec cummProb idx = if k > pcdf.[idx] then cummProb (idx - 1) else idx
    
    abs(cummProb pcdlen - pcdlen) 

let DiscreteSample p = cdf p |> getDiscreteSample  

///slope/beta , y-intercept/alpha, covariance, variance of x , variance of y
let inline simpleStats (vect1 : ^a seq) (vect2 : seq< ^a >) =   
    let vec, vec2 = Array.ofSeq vect1, Array.ofSeq vect2
    let xm, ym = vec |> Array.average , vec2 |> Array.average  
    let cov, varx,vary = (vec, vec2) ||> Array.fold2 (fun (cov, varx, vary) x y -> 
                                        cov + (x - xm) * (y - ym), 
                                        varx + (squared (x - xm)), 
                                        vary + (squared (y - ym))) 
                                    (Unchecked.defaultof<'a>, Unchecked.defaultof<'a>, Unchecked.defaultof<'a>) 
    let beta = cov/varx
    beta, ym - beta * xm, cov, varx, vary 

let meanDifference (v:float[]) = 
    let n, nl = float v.Length, v.Length 
    let M = recurse (fun (i,j,_) -> j = nl ) 
                    (fun (i,j, x) -> let i', j' = if i = nl - 1 then 0, j + 1 else i + 1, j
                                     i', j', x + abs (v.[i] - v.[j])) (0,0,0.) |> third
    M / (n * (n - 1.))  

let relativeMeanDiff (v:float[]) = let meandiff = meanDifference v in meandiff, meandiff / (v |> Array.average)    

let online_variance_mean variance mean_n n x =
    let mean_n' = mean_n + (x - mean_n) / n
    variance + (x - mean_n) * (x - mean_n'), mean_n', n + 1. 

let online_mean mean_n n x =
    let mean_n' = mean_n + (x - mean_n) / n
    mean_n', n + 1. 
///variance , mean
let varianceAndMean = function 
    | x when x = Seq.empty -> 0.0 , 0.0
    | l -> let mean = Seq.average l 
           (Seq.sumBy (fun x -> (x - mean)**2.) l)/ (float (Seq.length l)) , mean   
 
let varianceFromMean mean = function 
    | x when x = Seq.empty  -> 0.0 
    | l -> (Seq.sumBy (fun x -> (x - mean) ** 2.) l)/ (float (Seq.length l))    
               
let stddev data = varianceAndMean data |> snd |> sqrt

/// O(n log n) median returns if of even length, returns middle and penmiddelate objects
let inline medianGen (x: 'a [])= 
    let sorted = (x |> Array.sort)
    let xlen, xlenh = x.Length, x.Length / 2  
    xlen % 2 = 0, sorted.[xlenh],sorted.[xlenh - 1] 

let inline median (x: ^a [])= 
  if x.Length = 1 then float x.[0]
  else
    let iseven, med, medl = medianGen x
    if iseven then float(med + medl) / 2.
    else float med 

//***************************PERMUTATIONS AND CHANCE******************//

let facI n = [2I..n] |> List.fold (*) 1I

let permsI n k = facI n / facI (n - k)

let fac n = [2.0..n] |> List.fold (*) 1.

///num permutations of length n taken k at a time
let perms n k = fac n / fac (n - k)

///num permutations of length n taken k at a time where order does not matter
let combinations n k = fac n / (fac k * fac (n - k))

//This is brilliant! From Harrop's F# for Scientists
let rec powerset s = 
  seq {
    match s with
    | [] -> yield []
    | h::t -> for x in powerset t do yield! [x; h::x]
  }

// From: http://stackoverflow.com/questions/286427/calculating-permutations-in-f
let rec internal insertions x = function
    | []  -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

let genPermutations collection = 
    let rec permutations  = function
        | []      -> seq [ [] ]
        | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs)) 
    collection |> Seq.toList |> permutations 

type System.Random with
    member t.NextDouble(minim, maxim) =  t.NextDouble() * (maxim - minim) + minim 

///The X means cross. Cheesy. But stands for thread safe and fast Random Wrapper Class
//http://blogs.msdn.com/b/pfxteam/archive/2009/02/19/9434171.aspx
type RandomX() =   
  [<ThreadStatic;DefaultValue>] 
  static val mutable private _local : Random    

  static member checkInst () = 
    if RandomX._local = null then 
        let seed = ref 0
        lock (random) (fun () -> seed := random.Next())
        RandomX._local <- new Random(!seed)
    RandomX._local 

  static member Next(low,high) =   
    let x = RandomX.checkInst() //seems a bit faster this way
    x.Next(low,high)

  static member Next(max_) =   
    let x =RandomX.checkInst()
    x.Next(max_)

  static member Next() = 
    let x = RandomX.checkInst()
    x.Next() 

  static member NextDouble(minim,maxim) = 
    let x = RandomX.checkInst()
    x.NextDouble(minim,maxim)  

///Allows one to generate a stream of unique numbers. It is very inefficient if the required set of numbers
///is near the range of the random source.
type randomInt (rNext, lower,upper) =
  let visited = Collections.Generic.HashSet<int>() 
  member this.stream () = seq { while true do yield rNext(lower,upper)}
  member this.resetVisited () = visited.Clear()
  member this.uniqueStream () =
     seq{  
          while visited.Count < upper - lower do
           let next = this.stream () |> Seq.skipWhile (visited.Contains) |> Seq.head
           visited.Add next |> ignore
           yield next}
  member this.nextunique () = this.uniqueStream () |> Seq.head
  member this.next() = this.stream() |> Seq.head 

//***************************ROUNDING******************//

let inline round (places:int) (num: float) = Math.Round(num, places)

///buckets rounds a number to roundTo places then buckets with bucketSize. where all numbers are floored to last 
///multiple of bucketSize so for example
///bucketRange 2 5 n will round to 2 places and allow only multiples of 5. n = 11..14 -> 10
///and n = 15..19 -> 15. for 2 0.5 n, n = 21.0..21.4 -> 21 and n = 21.5..21.9 go to 21.5, integers are unaffected 
let inline bucketRange roundTo bucketSize m = 
   let num = (round roundTo m) 
   if num < bucketSize then 0. else num - num % bucketSize
   
///scaleTo scales a number to rmin and rmax where rangemin and max are the expected range of the number.
///examples: scaleTo -10 10 -1000. 1000. -10. = -0.1, a number with range
///-1000 to 1000 is fit to -10 by 10, scaleTo 0 10 0 100 50 = 5.
let scaleTo rmin rmax rangemin rangemax value =
   let adjrmin, adjrmax, adjval = if rangemin < 0. then 0., -rangemin + rangemax , -rangemin + value 
                                  else rangemin, rangemax , value //translate to 0
    
   (adjval - adjrmin)/(adjrmax - adjrmin) * (rmax-rmin) + rmin
   
//***************************Array Useful Vector Math******************//

let inline colAverageGen (numRows : 'b) (typefunc : 'a -> 'b) (rows : 'a[][]) =  
  let den = numRows 
  let outArr = Array.create rows.[0].Length (Unchecked.defaultof<'b>)
  for i in 0..rows.[0].Length - 1 do
       for j in 0..int den - 1 do
         outArr.[i] <- outArr.[i] + typefunc rows.[j].[i] / den
  outArr

type Array with
 static member inline shuffle (arr : 'a []) =  (arr |> Array.sortBy (fun _ -> random.Next())) 
 static member inline Op operator a b = Array.map2 operator a b                         //NOTE: in defaultof<>,no performance penalty
 static member inline dotproduct v1 v2 = Array.fold2 (fun dotp x1 x2 -> x1 * x2 + dotp) Unchecked.defaultof<'a> v1 v2
 static member inline magnitude v = Array.dotproduct v v |> sqrt 
 static member inline to_unitvector v = let mag = Array.magnitude v in v |> Array.map (flip (/) mag)
 static member inline normalize (data: ^a[]) = 
     let total = data |> Array.sum
     data |> Array.map (flip (/) total) 
 
  
 static member sampleOne (a:'a[]) = a.[random.Next(a.Length)]
 static member inline normalizeBy f (data: ^a []) = 
     let total = data |> Array.sumBy f
     data |> Array.map (f >> flip (/) total)

 static member inline cosineSimilarityMag tol v1 v2 mag1 mag2 =
   Array.dotproduct v1 v2  / ((mag1 * mag2) + tol)
   
 static member transposeJagged (a : 'a[][]) = 
     [|for c in 0..a.[0].Length - 1 -> 
         [|for r in 0..a.Length - 1 -> a.[r].[c]|]|]

 static member inline cosineSimilarity tol v1 v2 =
    Array.dotproduct v1 v2 / ((Array.magnitude v1 * Array.magnitude v2) + tol)

 static member inline colAverageFloats (rows:'a[][]) = colAverageGen (float rows.Length) float rows
 
type 'a ``[]`` with
 ///O(n) in place
 member arr.permuteYates () =
   let rec doswaps = function
     | 0 -> ()
     | n ->  let randN = random.Next(n + 1)
             swapArr n randN arr
             doswaps (n - 1)
   doswaps (arr.Length - 1) 

/////////////////////////////

let distCovariance (v1:float[]) (v2:float[]) same = 
    let n = v1.Length
    let nf = float n 
    let denum =  nf ** 2.  
    let distMatrix (v:float[]) = Array2D.init v.Length v.Length (fun k l -> abs (v.[k] - v.[l]))  
    let rowMean (m:float[,]) k = (m |> Array2D.foldAt 1 k (+) 0.)/nf  
    let colMean (m:float[,]) l = (m |> Array2D.foldAt 0 l (+) 0.)/nf               
    let matrixMean (m:float[,]) = (m |> Array2D.fold (+) 0.) / denum 

    let centredDist (v:float[]) = 
        let distm = distMatrix v
        let meanoverall = matrixMean distm   
        let C = Array2D.create n n 0.
        Threading.Tasks.Parallel.For(0, n, fun i -> 
                        let curRowMean = rowMean distm i
                        for j in 0..n - 1 do 
                            C.[i,j] <- distm.[i,j] - curRowMean - (colMean distm j) + meanoverall  ) |> ignore
        C
             
    let A,B  = 
            if same then let A2 = centredDist v1 in A2, A2   
            else let AB = [| async {return centredDist v1}
                             async {return centredDist v2} |] |> Async.Parallel |> Async.RunSynchronously 
                 AB.[0], AB.[1]
    let _,_,msum = A |> Array2D.fold (fun (i,j,curSum) value -> 
                                               let nsum = value * B.[i,j] + curSum  
                                               if j = n - 1 then i+1, 0,nsum else i, j+1,nsum) (0,0,0.)  
    msum  / denum    

///Distance Correlation is a useful method of calculating correlation with distance covariance that is able to pick up on non-linearities unlike Pearson's. And more flexible than Spearman's rank.
let distCorrelation v1 v2 = 
    let VXY = [| async { return distCovariance v1 v1 true }
                 async { return distCovariance v2 v2 true} |] |> Async.Parallel |> Async.RunSynchronously 
    let vsig = VXY.[0] * VXY.[1] 
    if vsig = 0. then 0. 
    else sqrt((distCovariance v1 v2 false) / sqrt vsig)    

//////////////////////////////////////// 
let toBase b n =  
   let logf x = log x / log b 
   let rec breaknum bs = 
     function
      | x when x <= 0. -> bs
      | x ->
       let p = x |> logf |> floor 
       let basen = b ** p
       let leading_digit = floor(x / basen)
       let next = x - (leading_digit * basen) 
       breaknum ((p,leading_digit)::bs) next
   breaknum [] n |> List.toArray

let readoutNum b = Array.map (fun (p,d) -> d * b ** p) 
 
let baseNumToString (l:'a[]) lmap =
    [|for i in 0.0..(fst l.LastElement) do 
        yield 
         (match Map.tryFind i lmap with
          | None -> "0"
          | Some d when d < 10. -> string d
          | Some d when d < 36.-> string(char (d+55.))
          | Some d -> string d + "|" )|] |> Array.rev |> joinToString

////////

let secondsToText = function 
    | 0. -> "Now"
    | x when x > 60. && x < 3600. -> let m = (x / 60.) |> round 2 in string m + " minutes"
    | x when x > 3600. && x <= 3600. * 24. -> ((x / 3600.) |> round 1 |> string) + " hours"
    | x when x > 3600. * 24. -> ((x / (3600. * 24.)) |> round 1 |> string) + " days"
    | x -> (x |> round 2 |> string) + " seconds"

let hoursToText = function
   | h when (h * 60.) < 1. -> "seconds", h / 3600. |> round 1
   | h when h < 1. -> "minutes", h * 60. |> round 1
   | h when h < 24. -> "hours", h |> round 1
   | h when h > 24. * 7. * 4. -> "months", round 2 (h/(24. * 7. * 4.))
   | h when h > 24. * 7. -> "weeks", round 2 (h/(24. * 7.))
   | h  -> "days", round 2 (h/24.)  

type DateTime with 
  member d.ToRoughDateString () = 
   let today = DateTime.Now.Date
   let span = (d.Date - today).TotalDays 
   if DateTime.Now >= d then "now"
   elif d.Date = today then d.ToShortTimeString()
   elif span = 1. then d.ToShortTimeString() + ", tomorrow"
   elif span > 1. && d.Day <= 28 then sprintf "the %dth, in %d days" d.Day (round 0 span |> int)
   else d.ToShortTimeString() + ", " + d.ToLongDateString()