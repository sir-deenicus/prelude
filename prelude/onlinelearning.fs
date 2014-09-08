module Prelude.Onlinelearning

open Prelude

open Prelude.Math
open System
open Prelude.Common

let rootMeanError data lbls vec = 
    (data, lbls) ||> Array.map2 (fun x y -> (Array.dotproduct vec x - y)**2.0) 
                 |> Array.average
                 |> sqrt

let inline logistic z =  1./(1.+ exp(z))

let internal regressStream alpha h op (xs : float []) (y:float) ws_ =
    let ws = defaultArg ws_ (Array.init xs.Length (fun _ -> random.NextDouble() + 0.0001))  
    let nw = h(Array.dotproduct xs ws) - y  
    for j in 0..(ws.Length - 1) do 
        ws.[j] <- op ws.[j] (alpha * nw * xs.[j])  
                        
    ws,nw

let internal regress alpha h op (xs : float [][]) (ys:float[]) ws_ =
    let ws = defaultArg ws_ (Array.init xs.[0].Length (fun _ -> random.NextDouble() + 0.0001))  
    let rec loop err = function 
        | n when n >= (ys.Length - 1) -> err
        | i -> let nw = h(Array.dotproduct xs.[i] ws) - ys.[i]  
               for j in 0..(ws.Length - 1) do 
                    ws.[j] <- op ws.[j] (alpha * nw * xs.[i].[j]) 
               loop nw (i + 1)
    let er = loop Double.MaxValue 0                        
    ws,er 

let logisticRegress a xs ys = regress a logistic (+) xs ys 

let logisticRegressStream a xs ys = regress a logistic (+) xs ys

let linearRegressWH a xs ys  = regress a id (-) xs ys 
 
let inline iterateLearner nmax f xs ys = 
    let w0 = f xs ys None
    recurse (fst >> (<=) nmax) 
            (fun (n,((w,lastErr) as oldWeight)) ->
                let (nw, e) = f xs ys (Some w)  
                let w2, e2 = if abs e <= lastErr then nw, abs e else oldWeight
                n + 1 , (w2,e2)
            ) (0, w0) 

////////////////

let inline regressPredict w x = Array.dotproduct w x
 
let logisticClassifyProb w x = 1./(1.+ exp(Array.dotproduct x w)) 

let logisticClassify x w = 
    let cl = 1./(1.+ exp(Array.dotproduct x w))
    if cl > 0.5 then 1. else 0.

let knn distfunc n data = 
 (data |> Array.map (fun (v,l) -> 
             let nears = data |> Array.map (fun (v2, l2) -> l2, distfunc v v2) 
                              |> Array.sortBy snd  
             l,  nears.[1..] |> Array.sub2 0 n |> Seq.mode))