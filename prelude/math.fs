module Prelude.Math
open Prelude.Common
open System

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

let random = new Random() 

let pi = Math.PI

let inline squared (x: ^a) = x * x 

/// O(n log n) median
let median x = 
    let sorted = (x |> Array.sort)
    let xlen, xlenh = x.Length, x.Length / 2
    if xlen % 2 = 0 then (sorted.[xlenh] + sorted.[xlenh - 1]) / 2.
    else sorted.[xlen / 2] 

let permute (arr : 'a []) =  (arr |> Array.sortBy (fun _ -> random.Next())) 

///O(n) in place
let permuteYates (arr : ' a[]) =
   let rec doswaps = function
     | 0 -> ()
     | n ->  let randN = random.Next(n + 1)
             swapArr n randN arr
             doswaps (n - 1)
   doswaps (arr.Length - 1) 
 
let inline round (places:int) (num: float) = Math.Round(num, places)

type System.Random with
    member t.NextDouble(minim, maxim) =  t.NextDouble() * (maxim - minim) + minim 

 
