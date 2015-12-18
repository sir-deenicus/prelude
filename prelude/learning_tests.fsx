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

////////////////////////////////

let buildLearnerWeights n f train lbls extr_weight =
   let _, (w,_) = iterateLearner n fst f train lbls

   extr_weight w

let runTest test lbls f =   
   let tstres = test |> Array.map f                       
   let correct = Array.zip tstres lbls |> Array.filter (fun (p,actual) -> p = actual)
   float correct.Length / float lbls.Length

/////////////////////////////////////

let perfLogRegAvg =
   time_this2 5 (fun t -> t.TotalSeconds)  (fun _ -> 
      let w = buildLearnerWeights 20 (logisticRegressAvg 0.01) train tlabels0 (snd >> Array.map fst)     
      runTest test tstslabels0 (logisticClassify w))        

//5 runs, 20 steps, 5.2 secs 0.83
perfLogRegAvg |> pairapply List.average

let perfLogReg =
   time_this2 5 (fun t -> t.TotalSeconds)  (fun _ -> 
      let w = buildLearnerWeights 20 (logisticRegress 0.01) train tlabels0 id  
      runTest test tstslabels0 (logisticClassify w))

//20 steps, 2.6 secs 0.79
perfLogReg |> pairapply List.average

let perfAvgPerceptron =
   time_this2 5 (fun t -> t.TotalSeconds)  (fun _ -> 
      let p = buildLearnerWeights 20 averagedPerceptronStep train tlabels id  
      let w = extractAveragedPerceptron p
      runTest test tstlabels (perceptronClassify w >> float))
                                                           

//20 steps, 1.5 secs 0.84  | 10 steps, 0.77 secs 0.81
perfAvgPerceptron |> pairapply List.average

let perfSVM =
   time_this2 5 (fun t -> t.TotalSeconds)  (fun _ -> 
      let w,_ = buildLearnerWeights 150 (pegasosOnlineSVM_Step 1e-4) train tlabels id    
      runTest test tstlabels (linearPredict w >> sign >> float))
                                                           

//20 steps, 3 secs 0.5...not working?, random guessing
perfSVM |> pairapply List.average

/////////////////////////////////////
let getdat maplbl dat = 
   let x,y = Array.unzip dat
   x , y |> Array.map maplbl



///////////////////////////////////

let ctg = pathCombine DocumentsFolder "ctg.csv" |> IO.File.ReadAllLines 
let data = ctg.[1..] |> Array.map (splitstrWith "," >> Array.map float) |> Array.shuffle 
   
   
let binarize = function x when x > 1. -> -1. | x -> x
                 
let traindat,tstdat = 
   let dat = Array.map (fun (d:float[]) -> d.[..d.Length - 1], binarize d.LastElement) data
   let _, ToInt smallest = dat |> Array.countElementsMapThenFilter snd (konst true) 
                               |> Seq.minBy keyValueToValue 
                               |> keyValueToPair
   dat 
   |> Seq.groupBy snd 
   |> Seq.collect (snd >> Seq.toArray >> Array.subOrMax smallest) 
   |> Seq.toArray //|> Array.countElementsMapThenFilter snd (konst true) |> Map.toArray |> Array.normalizeWeights  
   |> Array.shuffle
   |> Array.splitByPercent 0.7

let train2, lbl2 = traindat |> Array.unzip
let test2, lbltest2 = tstdat |> Array.unzip

let lbl2_0 = lbl2  |> Array.map (function -1. -> 0. | x -> x)
let lbltest2_0 = lbltest2  |> Array.map (function -1. -> 0. | x -> x)

Array.countElements lbl2 |> Map.toArray |> Array.normalizeWeights 
Array.countElements lbltest2 |> Map.toArray |> Array.normalizeWeights 
lbl2.Length, lbltest2.Length
/////////////////////////

let perfLogReg2 =
   time_this2 50 (fun t -> t.TotalSeconds)  (fun _ -> 
      let w = buildLearnerWeights 30 (logisticRegress 0.01) train2 lbl2_0 id  
      runTest test2 lbltest2_0 (logisticClassify w))

//50 runs of 30 steps; 0.551 secs, 0.842
//on balanced run: 0.696 (stddev 0.1)
perfLogReg2 |> pairapply (List.average >> round 3)
perfLogReg2 |> pairapply stddev //(0.08306660145, 0.0879010085)

let perfLogRegAvg2 =
   time_this2 50 (fun t -> t.TotalSeconds)  (fun _ -> 
      let w = buildLearnerWeights 30 (logisticRegressAvg 0.01) train2 lbl2_0 (snd >> Array.map fst)   
      runTest test2 lbltest2_0 (logisticClassify w))

//30 steps, 0.748 secs 0.857
//on balanced run: 0.812 (stddev 0.005)
perfLogRegAvg2 |> pairapply (List.average >> round 3)
perfLogRegAvg2 |> pairapply stddev // (0.06371003649, 0.001241677861)


let perfAvgPerceptron2 =
   time_this2 50 (fun t -> t.TotalSeconds)  (fun _ -> 
      let p = buildLearnerWeights 30 averagedPerceptronStep train2 lbl2 id  
      let w = extractAveragedPerceptron p
      runTest test2 lbltest2 (perceptronClassify w >> float))

//30 steps, 0.281 secs 0.857
//on balanced run: 0.812 (stddev 0.006)
perfAvgPerceptron2 |> pairapply (List.average >> round 3)
perfAvgPerceptron2 |> pairapply stddev // (0.0252207749, 0.001256279374)

//let ptrn = test2 |> Array.map (perceptronClassify w >> float >> function -1. -> 0. | x -> x)                                                          
//let lreg = test2 |> Array.map (logisticClassify w2)                                                          
//Array.zip ptrn lreg |> Array.filter (fun (x,y) -> x <> y) |> Array.length, ptrn.Length 
//avgperceptron vs avglogreg 5 disagreed out of 282, agree on 98%

let perfSVM2 =
   time_this2 50 (fun t -> t.TotalSeconds)  (fun _ -> 
      let w,_ = buildLearnerWeights 30 (pegasosOnlineSVM_Step 1e-2) train2 lbl2 id    
      runTest test2 lbltest2 (linearPredict w >> sign >> float))
                                                           
//30 steps, 0.482 secs 0.706...not working?one output only?
//on balanced run: 0.512  (stddev 0.04)
perfSVM2 |> pairapply (List.average >> round 3)
perfSVM2 |> pairapply stddev // (0.03165731701, 0.1550883151)

 
  