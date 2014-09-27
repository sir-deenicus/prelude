module Prelude.Onlinelearning

open Prelude

open Prelude.Math
open System
open Prelude.Common

let rootMeanError data lbls vec = 
    (data, lbls) ||> Array.map2 (fun x y -> (Array.dotproduct vec x - y)**2.0) 
                 |> Array.average
                 |> sqrt

let inline logistic x =  1./(1.+ exp(x))

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

let logisticRegressStream a xs ys = regressStream a logistic (+) xs ys

let linearRegressStream a xs ys = regressStream a id (-) xs ys

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

let inline linearPredict w x = Array.dotproduct w x
 
let logisticProbability w x = 1./(1.+ exp(Array.dotproduct x w)) 

let logisticClassify x w = 
    let cl = 1./(1.+ exp(Array.dotproduct x w))
    if cl > 0.5 then 1. else 0.

let knn distfunc n point data =  
    let nears = data |> Array.map (fun v2 -> v2, distfunc point v2) 
                     |> Array.sortBy snd  
    nears.[1..] |> Array.sub2 0 n 

////////

let initWeights f d n = Array.init n (fun _ -> Array.init d (fun _ -> random.NextDouble() |> f))

let inline initWeightsUnit f d n = 
    Array.init n 
     (fun _ -> 
       Array.init d (fun _ -> random.NextDouble() |> f)
       |> Array.to_unitvector)

let inline linearActivations weights example = 
  let ws = weights |> Array.mapi (fun i w -> i, Array.dotproduct w example)
  ws |> Array.maxBy snd

///takes the weights and a unitized example and shifts highest activation towards example
let inline online_kmeans_net nu (weights : 'a [] []) example =
     let bestw, dotp = linearActivations weights example
     for i in 0..weights.[bestw].Length - 1 do 
       let w_i = weights.[bestw].[i]
       weights.[bestw].[i] <- w_i + nu * (example.[i] - w_i)  

///maintains unity via division post update
let inline online_kmeans_sphere nu (weights : 'a [] []) example =
     let bestw, dotp = linearActivations weights example
     let topweight = weights.[bestw]

     for i in 0..topweight.Length - 1 do 
       topweight.[i] <- (topweight.[i] + nu * example.[i])
     
     let total = topweight |> Array.magnitude
     Array.iteri (fun i w -> topweight.[i] <- w / total) topweight

    
let buildDistTableSym f dist (v:'a[]) =
    let len = v.Length
    [|for i in 0..len - 1 -> i|] |> Array.Parallel.map 
        (fun i -> 
            [|for j in (i+1)..len - 1 ->
              f v.[i], f v.[j], (dist (f v.[i]) (f v.[j]))|])

let buildDistTable f dist (v1:'a[]) (v2:'a[]) = 
    [|for i in 0..v1.Length - 1 -> i|] |> Array.Parallel.map 
        (fun i -> 
            [|for j in 0..v2.Length - 1 ->
              (f v1.[i]), (f v2.[j]), dist (f v1.[i]) (f v2.[j])|])

