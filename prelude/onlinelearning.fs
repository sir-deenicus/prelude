module Prelude.Onlinelearning

open Prelude
open Prelude.Math
open System
open Prelude.Common

type AvgPerceptronState = {
   mutable C:float;
   mutable Bias : float;
   mutable ß : float
   Weights : float []
   CachedWeights : float  []
}

type AveragedWeights = {
    Weights : float []
    WeightsOnlineMean : (float * float) []
}     

let rootMeanError data lbls vec =
    (data, lbls)
    ||> Array.map2 (fun x y -> (Array.dotproduct vec x - y) ** 2.0)
    |> Array.average
    |> sqrt

let regress alpha h op weights (xs: float []) (y: float)  =
    let ws =
        defaultArg weights (Array.init xs.Length (fun _ -> random.NextDouble() + 0.0001))

    let e = h (Array.dotproduct xs ws) - y

    for j in 0 .. (ws.Length - 1) do
        ws.[j] <- op ws.[j] (alpha * e * xs.[j])

    ws, e

///weights, avgerageWeight (mean,n)
let regressAvg alpha h op weightXavgWeigths (xs : float []) (y : float) =
    let state =
        defaultArg weightXavgWeigths
            { Weights =
                  Array.init xs.Length (fun _ -> random.NextDouble() + 0.0001)
              WeightsOnlineMean = Array.init xs.Length (fun _ -> 0., 1.) }

    let ws, avgws = state.Weights, state.WeightsOnlineMean
    let e = h (Array.dotproduct xs ws) - y

    for j in 0..(ws.Length - 1) do
        ws.[j] <- op ws.[j] (alpha * e * xs.[j])
        let m, c = avgws.[j]
        avgws.[j] <- Stats.online_mean m c ws.[j]

    state, e

      
///params = (weight,cachedAvgWeigths, c, bias, beta)
let averagedPerceptronStep parameters (vec : float []) (y:float) =
    let param =
        defaultArg parameters { Weights =
                                    Array.init vec.Length
                                        (fun _ -> random.NextDouble() + 0.0001)
                                CachedWeights =
                                    Array.init vec.Length (fun _ -> 0.)
                                C = 0.
                                Bias = 0.
                                ß = 1. }

    let p = ((Array.dotproduct vec param.Weights) + param.Bias) * y

    if p <= 0. then 
      for j in 0..(param.Weights.Length - 1) do 
         param.Weights.[j] <- param.Weights.[j] + (y * vec.[j]) 
         param.CachedWeights.[j] <- param.CachedWeights.[j] + (y * param.C * vec.[j])

      param.Bias <- param.Bias + y
      param.ß <- param.ß + y * param.C   

    param.C <- param.C + 1.
    param,p               
     
let extractAveragedPerceptron parameters =
    Array.map2 (fun w u -> w - u / parameters.C) parameters.Weights
        parameters.CachedWeights, parameters.Bias - parameters.ß / parameters.C
      
let perceptronClassify (weight,b) x = sign(Array.dotproduct weight x + b)
      
let inline nlogistic x =  1./(1.+ exp(x))

let logisticRegress a ws xs y = regress a nlogistic (+) ws xs y    

let linearRegress a ws xs y = regress a id (-) ws xs y

///weights, avgerageWeight (mean,n)
let logisticRegressAvg a ws xs y = regressAvg a nlogistic (+) ws xs y

///weights, avgerageWeight (mean,n)
let linearRegressAvg a ws xs y = regressAvg a id (-) ws xs y                                          

let inline iterateLearner nmax getWeight f (xs : _ []) (ys : _ []) =
    let params0 = f None xs.[0] ys.[0]
    let indices = [| 1..xs.Length - 1 |]

    let rec loop n oldParams =
        if n >= nmax then oldParams
        else
            indices.permuteYates()
            indices
            |> Array.fold
                   (fun newparams i ->
                   f (Some(getWeight newparams)) xs.[i] ys.[i]) oldParams
            |> loop (n + 1)
                 
    loop 0 params0

////////////////

let inline linearPredict w x = Array.dotproduct w x

let logisticProbability w x = 1. / (1. + exp (Array.dotproduct x w))

let logisticClassify w x =
    let c = 1. / (1. + exp (Array.dotproduct x w))
    if c > 0.5 then 1. else 0.


let knn distfunc n point data =
    let nears =
        data
        |> Array.map (fun v2 -> v2, distfunc point v2)
        |> Array.sortBy snd

    nears.[..n]

////////

let initWeights f d n =
    Array.init n (fun _ -> Array.init d (fun _ -> random.NextDouble() |> f))

let inline initWeightsUniform def tounit d n =
    Array.init n (fun _ ->
        let a = Array.create d def
        if tounit then Array.to_unitvector a
        else a)

let inline initWeightsUnit f d n =
    Array.init n
        (fun _ ->
        Array.init d (fun _ -> random.NextDouble() |> f) |> Array.to_unitvector)

let inline linearActivations weights example =
    let ws = weights |> Array.mapi (fun i w -> i, Array.dotproduct w example)
    ws |> Array.maxBy snd


///takes the weights and a unitized example and shifts highest activation towards example
let inline online_kmeans_net nu (weights: 'a [] []) example =
    let bestw, _ = linearActivations weights example

    for i in 0 .. weights.[bestw].Length - 1 do
        let w_i = weights.[bestw].[i]
        weights.[bestw].[i] <- w_i + nu * (example.[i] - w_i)


///maintains unity via division post update
let inline online_kmeans_sphere nu (weights: 'a [] []) example =
    let bestw, _ = linearActivations weights example
    let topweight = weights.[bestw]

    for i in 0 .. topweight.Length - 1 do
        topweight.[i] <- (topweight.[i] + nu * example.[i])

    let mag = topweight |> Array.magnitude
    Array.iteri (fun i w -> topweight.[i] <- w / mag) topweight
         
let buildDistTableSym f dist (v : 'a []) =
    let len = v.Length
    [| for i in 0..len - 1 -> i |]
    |> Array.Parallel.map
           (fun i ->
           [| for j in (i + 1)..len - 1 ->
                  f v.[i], f v.[j], (dist (f v.[i]) (f v.[j])) |])

let buildDistTable f dist (v1 : 'a []) (v2 : 'a []) =
    [| for i in 0..v1.Length - 1 -> i |]
    |> Array.Parallel.map
           (fun i ->
           [| for j in 0..v2.Length - 1 ->
                  (f v1.[i]), (f v2.[j]), dist (f v1.[i]) (f v2.[j]) |])
          

