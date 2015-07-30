module Prelude.Onlinelearning

open Prelude

open Prelude.Math
open System
open Prelude.Common


let inline logistic x =  1./(1.+ exp(x))


let rootMeanError data lbls vec = 
    (data, lbls) ||> Array.map2 (fun x y -> (Array.dotproduct vec x - y)**2.0) 
                 |> Array.average
                 |> sqrt


let regressStream alpha h op (xs : float []) (y:float) ws_ =
    let ws = defaultArg ws_ (Array.init xs.Length (fun _ -> random.NextDouble() + 0.0001))  
    let e = h(Array.dotproduct xs ws) - y  
    for j in 0..(ws.Length - 1) do 
        ws.[j] <- op ws.[j] (alpha * e * xs.[j])   
    ws

///weights, avgerageWeight (mean,n)
let regressAvgStream alpha h op (xs : float []) (y:float) weightXavgWeigths =
    let ws,avgws = defaultArg weightXavgWeigths 
                              (Array.init xs.Length (fun _ -> random.NextDouble() + 0.0001)
                                ,Array.init xs.Length (fun _ -> 0. , 1.))
                     
    let e = h(Array.dotproduct xs ws) - y 
    
    for j in 0..(ws.Length - 1) do 
        ws.[j] <- op ws.[j] (alpha * e * xs.[j])  
        let m, c = avgws.[j]
        avgws.[j] <- online_mean m c ws.[j]            
    (ws,avgws)
            
///params = (weight,cachedAvgWeigths, c, bias, beta)
let averagedPerceptronStep alpha (vec : float []) (y:float) parameters =
    let ws,cachedws, c, b,ß = defaultArg parameters 
                                      (Array.init vec.Length (fun _ -> random.NextDouble() + 0.0001) 
                                       , Array.init vec.Length (fun _ -> 0.)
                                       , 0.,0.,1.)

    let p = ((Array.dotproduct vec ws) + b) * y
    let b', ß' = 
     if p <= 0. then 
        for j in 0..(ws.Length - 1) do 
            ws.[j] <- ws.[j] + (y * vec.[j]) 
            cachedws.[j] <- cachedws.[j] + (y * c * vec.[j])
        b + y, ß + y * c
     else b, ß
                        
    ws, cachedws,c + 1., b',ß' 
     
let extractAveragedPerceptron (weights, cachedweights, c:float, b, beta) = 
      Array.map2 (fun w u -> w - u/c) weights cachedweights, b - beta/c      
      
      
let perceptronClassify b weight x = sign(Array.dotproduct weight x + b)
                                        

let logisticRegress a vec y = regressStream a logistic (+) vec y


let linearRegress a vec y = regressStream a id (-) vec y

///weights, avgerageWeight (mean,n)
let logisticRegressAvg a vec y = regressAvgStream  a logistic (+) vec y

///weights, avgerageWeight (mean,n)
let linearRegressAvg a vec y = regressAvgStream a id (-) vec y                                          
 
let inline iterateLearner nmax f (xs:_[]) (ys:_[]) = 
    let params0 = f xs.[0] ys.[0] None
    let indices = [|0..xs.Length-1|]
    recurse (fst >> (<=) nmax) 
            (fun (n, oldParams) ->
                indices.permuteYates ()
                n + 1
                 , indices 
                   |> Array.fold (fun newparams i ->   
                        f xs.[i] ys.[i] (Some newparams)) oldParams  
            ) (0,params0) 

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


let inline initWeightsUniform def tounit d n = 
   Array.init n 
     (fun _ -> 
      let a = Array.create d def 
      if tounit then a |> Array.to_unitvector 
      else a) 


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

