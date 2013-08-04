module Prelude.Math

open System.Threading.Tasks
open Prelude.Common
open System

let random = new Random() 

let pi = Math.PI 

let inline squared (x: ^a) = x * x  

let log2 = log >> (flip (/) (log 2.)) 

let inline addInPlaceIntoFirst (a1:'a []) (a2:'a[]) =
    for i in 0..a1.Length - 1 do 
       a1.[i] <- a1.[i] + a2.[i]

//***************************BASIC STATS******************//

///slope/beta , y-intercept/alpha, covariance, variance of x , variance of y
let simpleStats (vec : float[]) (vec2 : float[]) =   
    let xm, ym = vec |> Array.average , vec2 |> Array.average  
    let cov, varx,vary = (vec, vec2) ||> Array.fold2 (fun (cov, varx, vary) x y -> cov + (x - xm) * (y - ym), varx + (x - xm) ** 2., vary + (y - ym) **2. ) (0. , 0., 0.) 
    let beta = cov/varx
    beta, ym - beta * xm, cov, varx, vary 

let meanDifference (v:float[]) = 
    let n, nl = float v.Length, v.Length 
    let M = fold (fun (i,j,_) -> j = nl ) 
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

let varianceAndMean = function 
    | x when x = Seq.empty -> 0.0 , 0.0
    | l -> let mean = Seq.average l 
           (Seq.sumBy (fun x -> (x - mean)**2.) l)/ (float (Seq.length l)) , mean   
 
let varianceFromMean mean = function 
    | x when x = Seq.empty  -> 0.0 
    | l -> (Seq.sumBy (fun x -> (x - mean) ** 2.) l)/ (float (Seq.length l))    
               
let stddev l = varianceAndMean l |> snd |> sqrt

/// O(n log n) median
let inline median (x: ^a [])= 
    let sorted = (x |> Array.sort)
    let xlen, xlenh = x.Length, x.Length / 2  
    if xlen % 2 = 0 then float(sorted.[xlenh] + sorted.[xlenh - 1]) / 2.
    else float(sorted.[xlen / 2]) 

//***************************PERMUTATIONS******************//

let permute (arr : 'a []) =  (arr |> Array.sortBy (fun _ -> random.Next())) 

///O(n) in place
let permuteYates (arr : ' a[]) =
   let rec doswaps = function
     | 0 -> ()
     | n ->  let randN = random.Next(n + 1)
             swapArr n randN arr
             doswaps (n - 1)
   doswaps (arr.Length - 1) 

type System.Random with
    member t.NextDouble(minim, maxim) =  t.NextDouble() * (maxim - minim) + minim 

//***************************ROUNDING******************//

let inline round (places:int) (num: float) = Math.Round(num, places)

///buckets rounds a number to roundTo places then buckets with bucketSize. where all numbers are floored to last 
///multiple of bucketSize so for example
///bucketRange 2 5 n will round to 2 places and allow only multiples of 5. n = 11..14 -> 10
///and n = 15..19 -> 15. for 2 0.5 n, n = 21.0..21.4 -> 21 and n = 21.5..21.9 go to 21.5, integers are unaffected 
let inline bucketRange roundTo bucketSize m = 
   let num = (round roundTo m) 
   if num < bucketSize then num else num - num % bucketSize

///scaleTo scales a number to rmin and rmax where rangemin and max are the expected range of the number.
///examples: scaleTo -10 10 -1000. 1000. -10. = -0.1, a number with range
///-1000 to 1000 is fit to -10 by 10, scaleTo 0 10 0 100 50 = 5.
let scaleTo rmin rmax rangemin rangemax value =
   let adjrmin, adjrmax, adjval = if rangemin < 0. then 0., -rangemin + rangemax , -rangemin + value 
                                  else rangemin, rangemax , value //translate to 0
    
   (adjval - adjrmin)/(adjrmax - adjrmin) * (rmax-rmin) + rmin
   
//***************************Array Useful Vector Math******************//

type Array with
 static member inline Op operator a b = Array.map2 operator a b                         //NOTE: in defaultof<>,no performance penalty
 static member inline dotproduct v1 v2 = Array.fold2 (fun dotp x1 x2 -> x1 * x2 + dotp) Unchecked.defaultof<'a> v1 v2
 static member inline magnitude v = Array.dotproduct v v |> sqrt 
 static member inline cosineSimilarity v1 v2 tol =
    Array.dotproduct v1 v2 / ((Array.magnitude v1 * Array.magnitude v2) + tol)

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


 
