namespace Prelude

open Prelude.Common
open Prelude.Math
 
module TextHistogram =
    let inline genericHistogram toFloat len dat =
        let d = Seq.toArray dat
        let stringr100 n x = string (round n (x * 100.))
        let _, maxp = Array.maxBy snd d
        Array.map (fun (x, prob) -> 
              let p = toFloat prob
              [|sprintf "%A" x ;
                stringr100 2 p + "%"; 
                String.replicate (int (round 0 (p / (toFloat maxp) * (float len)))) "█" |]) d
        |> makeTable "\n" [|"item";"p"; ""|] ""  

    let histogram len dat = genericHistogram id len dat 

module Sampling =  

    let discreteSampleIndex (prob : float []) =  
        let cummulativeDistr = Array.create prob.Length prob.[0]  

        for i in 1..prob.Length - 1 do
            cummulativeDistr.[i] <- cummulativeDistr.[i-1] + prob.[i] 

        let k = randomx.NextDouble() * cummulativeDistr.[^0]

        let rec cummProb index =
            if k > cummulativeDistr.[index] then cummProb (index + 1)
            else index
        cummProb 0  

    let inline cdf (prob : _ []) =
        let cd = Array.create prob.Length prob.[0]  
        for i in 1..prob.Length - 1 do 
            let (x, p) = prob.[i]
            cd.[i] <- (x, snd cd.[i-1] + p)
        cd 

    let sampleFromCummulative (cummulativeDistr : ('a * float) []) =
        let k = randomx.NextDouble() * (snd cummulativeDistr.[^0]) 
    
        let rec cummProb index =
            if k > snd cummulativeDistr.[index] then cummProb (index + 1)
            else index, cummulativeDistr.[index] 
        let i, (item,_) = cummProb 0 
        i, item
    
    let inline discreteSample p = sampleFromCummulative (cdf p) |> snd

    let discreteSampleN n items = [|for _ in 1..n -> discreteSample items|]

    let sampleNoReplacement n items =
        let rec sampleN_without_replacement i ch =
            function
            | _ when i >= n -> ch
            | [] -> ch
            | choices ->
                let choice = discreteSample (List.toArray choices) 
                let rem = List.filter (fst >> (<>) choice) choices
                sampleN_without_replacement (i + 1) (choice::ch) rem
        sampleN_without_replacement 0 [] items
    
    let sampleNoReplacementWithSampler sampler n items =
        let rec sampleWithoutReplacement i ch =
            function
            | _ when i >= n -> ch
            | [] -> ch
            | choices ->
                let choiceIndex = sampler choices
                let mutable c = -1
                let mutable found = None
    
                let rem =
                    [ for x in choices do
                        c <- c + 1
                        if c <> choiceIndex then yield x
                        else found <- Some x ]
                sampleWithoutReplacement (i + 1) ((choiceIndex, found.Value)::ch) rem
        sampleWithoutReplacement 0 [] items


    type DiscreteSampler<'a>(itemProbabilities : ('a * float) seq) =   

        let mutable cumulativeDist = cdf (Seq.toArray itemProbabilities) 

        member __.ChangeDistributionWith ps = cumulativeDist <- cdf (Seq.toArray ps)

        member __.DrawSamples(n) = 
            [|for _ in 1..n -> sampleFromCummulative cumulativeDist |> snd|] 

        member __.DrawSample() = sampleFromCummulative cumulativeDist    
         
          
module SampleSummarize =
    let smoothDistr alpha data =
        data
        |> Array.fold (fun (p_prior, ps) (x, p) ->
               let p' = Stats.exponentialSmoothing id alpha p_prior p
               p', (x, p') :: ps) (snd data.[0], [])
        |> snd
        |> List.toArray
        |> Array.normalizeWeights

    let inline computeDistAverage transform d = Array.sumBy (fun (x, p) -> transform x * p) d

    let computeCredible sortBy roundpsTo p1 p2 dat =
        let rec loop cumulativeProb ps =
            function
            | [] -> ps, cumulativeProb
            | _ when cumulativeProb > p2 -> ps, cumulativeProb
            | (x, p) :: dat ->
                let ps' =
                    if cumulativeProb + p < p1 then ps
                    else ((x, p, round roundpsTo (p + cumulativeProb)) :: ps)
                loop (cumulativeProb + p) ps' dat

        let cump, _ = loop 0. [] (List.ofSeq dat |> List.sortBy sortBy)
        cump, List.minBy third cump |> fst3, List.maxBy third cump |> fst3

    let getLargeProbItems maxp data =
        let rec innerloop curritems cumulativeprob =
            function
            | [] -> curritems
            | _ when cumulativeprob > maxp -> curritems
            | ((_, p) as item :: ps) ->
                innerloop (item :: curritems) (p + cumulativeprob) ps
        innerloop [] 0. data

    let findTopItem (vc : _ []) =
        let topindex, _ =
            vc
            |> Array.indexed
            |> Array.maxBy (snd >> snd)

        let (_, p) as topitem = vc.[topindex]
        topindex, p, [ topitem ]

    let getBulk (minp : float) items =
        let rec loopinner cmin cmax bulkMass sum =
            if sum > minp || (cmin < 0 && cmax >= Array.length items) then
                sum, bulkMass
            else
                let bulkMass' =
                    let frontpart =
                        if cmin < 0 then bulkMass
                        else items.[cmin] :: bulkMass
                    if cmax > items.Length - 1 then frontpart
                    else items.[cmax] :: frontpart

                let currentSum = List.sumBy snd bulkMass'
                loopinner (cmin - 1) (cmax + 1) bulkMass' currentSum

        let topindex, p, root = findTopItem items
        loopinner (topindex - 1) (topindex + 1) root p

    let getBulkAlternating (minp : float) toggle items =
        let rec loopinner toggle cmin cmax bulkMass sum =
            if sum > minp || (cmin < 0 && cmax >= Array.length items) then
                sum, bulkMass
            else
                let cmin', cmax', bulkMass' =
                    match toggle with
                    | true ->
                        cmin, cmax + 1, 
                            (if cmax > items.Length - 1 then bulkMass
                             else items.[cmax] :: bulkMass)
                    | false ->
                        cmin - 1, cmax, 
                           (if cmin >= 0 then items.[cmin] :: bulkMass
                            else bulkMass)

                let currentSum = List.sumBy snd bulkMass'
                loopinner (not toggle) cmin' cmax' bulkMass' currentSum

        let topindex, p, root = findTopItem items
        loopinner toggle (topindex - 1) (topindex + 1) root p

    let roundAndGroupSamplesWith f samples =
        samples
        |> Seq.toArray
        |> Array.map f
        |> Array.groupBy id
        |> Array.map (keepLeft (Array.length >> float))
        |> Array.normalizeWeights
    
    let compactMapSamplesAgg aggregator f samples =
        Array.map (fun (x, p : float) -> f x, p) samples
        |> Array.groupBy fst
        |> Array.map (fun (x, xs) -> x, aggregator snd xs)
        |> Array.normalizeWeights
      
    let compactMapSamplesSum f samples =
        compactMapSamplesAgg Array.sumBy f samples

    let compactMapSamplesAvg f samples =
        compactMapSamplesAgg Array.averageBy f samples


module ProbabilityTools =     
    let filterWith f data = 
        let matches = data |> Array.filter f
        (Array.length matches |> float) / (float data.Length) 
       
    let filterWithCondition conditional f data =
        let sub = Array.filter conditional data
        let matches = sub |> Array.filter f
        (Array.length matches |> float) / (float sub.Length)

    let inline mapDistribution projectTo m = 
        m
        |> Array.map (fun (x,p) -> projectTo x,p) 
        |> Array.groupBy fst 
        |> Array.map (fun (x,xs) -> x, Array.sumBy snd xs) 
        
    let inline probabilityOf filter m =
        Array.sumBy snd (Array.filter (fun (k, _) -> filter k) m)
        
    let inline conditionalProbability conditional matchwith m = 
        let sub = Array.filter (fun (k, _) -> conditional k) m
        let matches = Array.filter (fun (k,_) -> matchwith k) sub
        (Array.sumBy snd matches) / (Array.sumBy snd sub)
         
    let toBits x = x / log 2. 

    let inline log0 x =
        let zero = Unchecked.defaultof<'a>
        if x = zero then zero
        else log x   

    let inline entropy dist = -Seq.sumBy (fun (_, p) -> p * log0 p) dist
   
    let inline mutualInformation (joint:_ []) =
        joint |> Array.map (fun ((x,y), pxy) ->
            let px = probabilityOf (fst >> (=) x) joint
            let py = probabilityOf (snd >> (=) y) joint 
            
            pxy * log0(pxy/(px * py))) 
   
    let inline kldivergence (pA: _ []) (pB: _ []) =
        pA |> Array.map (fun (x, p_a) ->        
            let p_b = probabilityOf ((=) x) pB
            p_a * log0(p_a/ p_b)) 

    let inline betaMean (a, b) = a / (a + b)

    let inline dirichletMean (a : Map<_, _>) =
        let total = Map.sum a
        Map.map (fun _ a_i -> a_i / total) a

    let categoricalProbabilityOf (ps:Map<_,_>) x =
        match Map.containsKey x ps with 
        | false -> Map.add x 0.5 ps
        | true -> ps
        |> Map.toArray
        |> probabilityOf ((=) x)
        
    let updateBeta (a, b) t =
        if t then (a + 1., b)
        else (a, b + 1.)

    let updateDirichlet (m : Map<_, _>) x = Map.addOrUpdate m x ((+) 1.) 1.5 //prior = 0.5
    