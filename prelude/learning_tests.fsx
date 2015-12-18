#load "prelude.fs"
#load "math.fs"
#load "onlinelearning.fs"

open Prelude.Common
open Prelude.Math
open Prelude.Onlinelearning
open System

#time "on"

let loadData maplabelTo data lbls = 
      pathCombine DocumentsFolder data
      |> IO.File.ReadAllLines
      |> Array.map (fun d -> splitwSpace d |> Array.map float), 

      pathCombine DocumentsFolder lbls
            |> IO.File.ReadAllLines
            |> Array.map maplabelTo

let train, tlabels = loadData float "arcene_train.data" "arcene_train.labels"
let tlabels0 = tlabels |> Array.map (function -1. -> 0. | _ -> 1. )
train.Length

let test, tstlabels = loadData float "arcene_valid.data" "arcene_valid.labels"
let tstslabels0 = tstlabels |> Array.map (function -1. -> 0. | _ -> 1. )



let buildLearnerWeights n f lbls extr_weight =
   let _, (w,_) = iterateLearner n fst f train lbls

   extr_weight w

let runTest lbls f =   
   let tstres = test |> Array.map f                       
   let correct = Array.zip tstres lbls |> Array.filter (fun (p,actual) -> p = actual)
   float correct.Length / float lbls.Length


let perfLogRegAvg =
   time_this2 5 (fun t -> t.TotalSeconds)  (fun _ -> 
      let w = buildLearnerWeights 20 (logisticRegressAvg 0.01) tlabels0 (snd >> Array.map fst)     
      runTest tstslabels0 (logisticClassify w))        

//5 runs, 20 steps, 5.2 secs 0.83
perfLogRegAvg |> pairapply List.average

let perfLogReg =
   time_this2 5 (fun t -> t.TotalSeconds)  (fun _ -> 
      let w = buildLearnerWeights 20 (logisticRegress 0.01) tlabels0 id  
      runTest tstslabels0 (logisticClassify w))

//20 steps, 2.6 secs 0.79
perfLogReg |> pairapply List.average


let perfAvgPerceptron =
   time_this2 5 (fun t -> t.TotalSeconds)  (fun _ -> 
      let p = buildLearnerWeights 20 averagedPerceptronStep tlabels id  
      let w = extractAveragedPerceptron p
      runTest tstlabels (perceptronClassify w >> float))
                                                           

//20 steps, 1.5 secs 0.84  | 10 steps, 0.77 secs 0.81
perfAvgPerceptron |> pairapply List.average


let perfSVM =
   time_this2 5 (fun t -> t.TotalSeconds)  (fun _ -> 
      let w,_ = buildLearnerWeights 20 (pegasosOnlineSVM_Step 1e-4) tlabels id    
      runTest tstlabels (linearPredict w >> sign >> float))
                                                           

//20 steps, 3 secs 0.5...not working, random guessing
perfSVM |> pairapply List.average
