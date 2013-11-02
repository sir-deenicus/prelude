module Prelude.Onlinelearning

open Prelude

open Prelude.Math
open System
open Prelude.Common

let inline logistic z =  1./(1.+ exp(z))

let kmeans k distfunc (xs : float [][]) ws_ = 
    let ws = defaultArg ws_ ([|for i in 1..k -> i-1, 0., Array.init xs.[0].Length (fun _ -> random.NextDouble() + 0.0001)|])
  
    let rec loop = function 
        | n when n >= (xs.Length - 1) -> ()
        | i ->  let j, nk,bestk = ws |> Array.minBy (third >> distfunc)
                (xs.[i], bestk) ||> Array.iteri2 (fun j x w -> bestk.[j] <- w + 1./(nk + 1.) * (x - w))
                ws.[j] <- (j, nk + 1., bestk)    
              
                loop (i + 1)
    loop 0                        
    ws 
  
let regress alpha h op (xs : float [][]) (ys:float[]) ws_ =
    let ws = defaultArg ws_ (Array.init xs.[0].Length (fun _ -> random.NextDouble() + 0.0001))  
    let rec loop err = function 
        | n when n >= (ys.Length - 1) -> err
        | i -> let nw =  h(Array.dotproduct xs.[i] ws) - ys.[i]  
               for j in 0..(ws.Length - 1) do 
                    ws.[j] <- op ws.[j] (alpha * nw * xs.[i].[j])
            
               loop nw (i + 1)
    let er = loop Double.MaxValue 0                        
    ws,er 

let logisticRegress a xs ys = regress a logistic (+) xs ys 

let linearRegressWH a xs ys  = regress a id (-) xs ys 
 
let inline iterateLearner nmax f xs ys = 
    let w0 = f xs ys None
    recurse (fst >> (<=) nmax) (fun (n,((w,lastErr) as oldWeight)) ->
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
