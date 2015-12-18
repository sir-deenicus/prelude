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

///////////////////////////////////

let ctg = pathCombine DocumentsFolder "ctg.csv" |> IO.File.ReadAllLines 
let data = ctg.[1..] |> Array.map (splitstrWith "," >> Array.map float) |> Array.shuffle 
   
   
let binarize = function x when x > 1. -> -1. | x -> x
 
let balanceData dat =
   let _, ToInt smallest = dat |> Array.countElementsMapThenFilter snd (konst true) 
                               |> Seq.minBy keyValueToValue 
                               |> keyValueToPair
   dat 
   |> Seq.groupBy snd 
   |> Seq.collect (snd >> Seq.toArray >> Array.subOrMax smallest) 
   |> Seq.toArray //|> Array.countElementsMapThenFilter snd (konst true) |> Map.toArray |> Array.normalizeWeights  
   |> Array.shuffle
                   
let traindat,tstdat = 
   let dat = Array.map (fun (d:float[]) -> d.[..d.Length - 2], binarize d.LastElement) data
   dat 
   |> balanceData
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

//50 runs of 30 steps; 0.548 secs, 0.812
//on balanced run: 0.71 (stddev 0.1)
perfLogReg2 |> pairapply (List.average >> round 3)
perfLogReg2 |> pairapply stddev // (0.05803115049, 0.1092941724)

let perfLogRegAvg2 =
   time_this2 50 (fun t -> t.TotalSeconds)  (fun _ -> 
      let w2 = buildLearnerWeights 30 (logisticRegressAvg 0.01) train2 lbl2_0 (snd >> Array.map fst)   
      runTest test2 lbltest2_0 (logisticClassify w2)
      )

//30 steps, 0.681 secs 0.856
//on balanced run: 0.861 (stddev 0.005)
perfLogRegAvg2 |> pairapply (List.average >> round 3)
perfLogRegAvg2 |> pairapply stddev //(0.1123434969, 0.001142443039)


let perfAvgPerceptron2 =
   time_this2 50 (fun t -> t.TotalSeconds)  (fun _ -> 
      let p = buildLearnerWeights 30 averagedPerceptronStep train2 lbl2 id  
      let w = extractAveragedPerceptron p
      runTest test2 lbltest2 (perceptronClassify w >> float)
      )

//30 steps, 0.229 secs 0.857
//on balanced run: 0.862 (stddev 0.004)
perfAvgPerceptron2 |> pairapply (List.average >> round 3)
perfAvgPerceptron2 |> pairapply stddev // (0.03482199389, 0.001158296185)

//let ptrn = test2 |> Array.map (perceptronClassify w >> float >> function -1. -> 0. | x -> x)                                                          
//let lreg = test2 |> Array.map (logisticClassify w2)                                                          
//Array.zip ptrn lreg |> Array.filter (fun (x,y) -> x <> y) |> Array.length, ptrn.Length 
//avgperceptron vs avglogreg 4 disagreed out of 282, agree on 98.6%

let perfSVM2 =
   time_this2 50 (fun t -> t.TotalSeconds)  (fun _ -> 
      let w,_ = buildLearnerWeights 30 (pegasosOnlineSVM_Step 1e-2) train2 lbl2 id    
      runTest test2 lbltest2 (linearPredict w >> sign >> float))
                                                           
//30 steps, 0.504 secs 0.698...not working?one output only?
//on balanced run: 0.518  (stddev 0.04)
perfSVM2 |> pairapply (List.average >> round 3)
perfSVM2 |> pairapply stddev //(0.0412577871, 0.1739033789)

//////////////

let buildData2 transform fname =
   let praw = IO.File.ReadAllLines(pathCombine DocumentsFolder fname) |> Array.map (splitstrWith "," >> (Array.map float)) 
   let pdata_, ptest_ = praw |> Array.map (fun d -> d.[..d.Length - 2], d.LastElement) |> Array.shuffle |> transform |> Array.splitByPercent 0.7  
   pdata_ |> Array.unzip, ptest_ |> Array.unzip 

let (cdata, clbl), (ctst, ctlbl) = buildData2 id "concreg.csv"

let _, (wLin,eLin)  = iterateLearner 20 fst (linearRegress 0.000001) cdata clbl 
rootMeanError ctst ctlbl wLin //25.

let (_,wLinA),e  = iterateLearner 20 fst (linearRegressAvg 0.000001) cdata clbl |> snd
rootMeanError ctst ctlbl (wLinA |> Array.map fst) //13
/////////////////////////
//---- 

let (pdata, plbl), (ptst, ptlbl) = buildData2 balanceData "pima-indians-diabetes.data"

let (plbl1, ptlbl1) = pairapply (Array.map (function 0. -> -1. | x -> x)) (plbl, ptlbl)

Array.countElements plbl |> Map.toArray |> Array.normalizeWeights 
Array.countElements ptlbl |> Map.toArray |> Array.normalizeWeights 

//----
let perfLogReg3 =
   time_this2 50 (fun t -> t.TotalSeconds)  (fun _ -> 
      let w = buildLearnerWeights 30 (logisticRegress 0.1) pdata plbl id  
      runTest ptst ptlbl (logisticClassify w)
      )

//50 runs of 30 steps; 0.059 secs, 0.559
perfLogReg3 |> pairapply (List.average >> round 3)
perfLogReg3 |> pairapply stddev //(0.01355556871, 0.04737481702)
//-------------

let perfLogRegAvg3 =
   time_this2 50 (fun t -> t.TotalSeconds)  (fun _ -> 
      let w2 = buildLearnerWeights 30 (logisticRegressAvg 0.1) pdata plbl (snd >> Array.map fst)   
      runTest ptst ptlbl (logisticClassify w2)
      )

//30 steps, 0.069 secs 0.652             
perfLogRegAvg3 |> pairapply (List.average >> round 3)
perfLogRegAvg3 |> pairapply stddev //(0.002863445288, 0.007993863624)
//-------------

let perfAvgPerceptron3 =
   time_this2 50 (fun t -> t.TotalSeconds)  (fun _ -> 
      let p = buildLearnerWeights 30 averagedPerceptronStep pdata plbl1 id  
      let w = extractAveragedPerceptron p
      runTest ptst ptlbl1 (perceptronClassify w >> float)
      )

//30 steps, 0.038 secs 0.653              
perfAvgPerceptron3 |> pairapply (List.average >> round 3)
perfAvgPerceptron3 |> pairapply stddev // (0.01078171275, 0.008819875776)

//-------------
let perfSVM3 =
   time_this2 50 (fun t -> t.TotalSeconds)  (fun _ -> 
      let w,_ = buildLearnerWeights 30 (pegasosOnlineSVM_Step 1e-2) pdata plbl1 id    
      runTest ptst ptlbl1 (linearPredict w >> sign >> float))
                                                           
//30 steps, 0.052 secs 0.554
perfSVM3 |> pairapply (List.average >> round 3)
perfSVM3 |> pairapply stddev //(0.005431053837, 0.04582217892)


//////////////////////////////////




 
  