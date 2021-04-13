module Prelude.Math

open System.Threading.Tasks
open Prelude.Common
open System

let random = new Random()

let pi = Math.PI

let inline pow x y = Math.Pow(float x, float y)

let inline squared (x : ^a) = x * x

let inline log2 x = log x / (log 2.)

let nearestPow2 x =
    let n = log2 x |> ceil
    2. ** n

let inline isPowerOf2 (ToFloat x) =
    let y = log2 x
    round 8 (y - floor y) = 0.

//***************************BASIC STATS******************//
module Stats =
    type SummaryStats =
        { Mean : float
          Sum : float
          Stddev : float
          N : int
          Min : float
          Max : float
          Median : float }

    type SimpleStatsInfo =
        { Slope : float
          Intercept : float
          Covariance : float
          VarianceX : float
          VarianceY : float }

    let pearsonsCorr (stats : SimpleStatsInfo) =
        stats.Covariance / (sqrt stats.VarianceX * sqrt stats.VarianceY)

    ///slope/beta , y-intercept/alpha, covariance, variance of x , variance of y
    let inline simpleStats f (vect1 :float seq) (vect2 : seq<float>) =
        let vec, vec2 = Array.ofSeq vect1, Array.ofSeq vect2
        let xm, ym = vec |> Array.average, vec2 |> Array.average
        let cov_, varx_, vary_ =
            (vec, vec2)
            ||> Array.fold2
                    (fun (cov, varx, vary) x y ->
                    cov + (x - xm) * (y - ym), varx + (squared (x - xm)),
                    vary + (squared (y - ym)))
                    (0., 0., 0.)
        let len = float vec.Length
        let cov, varx, vary = cov_ / len, varx_ / len, vary_ / len
        let beta = cov / varx
        { Slope = beta
          Intercept = ym - beta * xm
          Covariance = cov
          VarianceX = varx
          VarianceY = vary }

    let meanDifference (v : float []) =
        let n, nl = float v.Length, v.Length

        let M =
            recurse (fun (i, j, _) -> j = nl) (fun (i, j, x) ->
                let i', j' =
                    if i = nl - 1 then 0, j + 1
                    else i + 1, j
                i', j', x + abs (v.[i] - v.[j])) (0, 0, 0.)
            |> third
        M / (n * (n - 1.))

    let relativeMeanDiff (v : float []) =
        let meandiff = meanDifference v
        meandiff, meandiff / (v |> Array.average)

    let online_variance_mean variance mean_n n x =
        let mean_n' = mean_n + (x - mean_n) / n
        variance + (x - mean_n) * (x - mean_n'), mean_n', n + 1.

    ///m12 = n = means = 0
    ///| E(X),E(Y), covXY,n
    let online_covariance (mean_n, mean2_n, m12, n) (x, y) =
        let n' = n + 1.
        let delta1 = (x - mean_n) / n'
        let delta2 = (y - mean2_n) / n'
        let m12' = m12 + (n' - 1.) * delta1 * delta2 - m12 / n'
        let mean_n', mean2_n' = mean_n + delta1, mean2_n + delta2
        mean_n', mean2_n', m12', n'

    let online_mean mean_n n x =
        let mean_n' = mean_n + (x - mean_n) / n
        mean_n', n + 1.

    ///variance , mean
    let varianceAndMean =
        function
        | x when x = Seq.empty -> 0.0, 0.0
        | l ->
            let mean = Seq.average l
            (Seq.sumBy (fun x -> (x - mean) ** 2.) l) / (float (Seq.length l)),
            mean

    ///variance , mean
    let varianceAndMeanf32 =
        function
        | x when x = Seq.empty -> 0.0f, 0.0f
        | l ->
            let mean = Seq.average l
            (Seq.sumBy (fun x -> (x - mean) ** 2.f) l) / (float32 (Seq.length l)),
            mean

    let varianceFromMean mean =
        function
        | x when x = Seq.empty -> 0.0
        | l ->
            (Seq.sumBy (fun x -> (x - mean) ** 2.) l) / (float (Seq.length l))

    let stddev data =
        varianceAndMean data
        |> fst
        |> sqrt

    /// O(n log n) median returns if of even length, returns middle and penmiddelate objects

    let inline medianx two (x : ^a []) =
        if x.Length = 1 then x.[0]
        else
            let iseven, med, medl = 
                let sorted = Array.sort x
                let xlen, xlenh = x.Length, x.Length / 2
                xlen % 2 = 0, sorted.[xlenh], sorted.[xlenh - 1]
    
            if iseven then (med + medl) /two
            else med
    
    let medianf32 x = medianx 2f x
    
    let median x = medianx 2. x 
    
    let standardize (w: float32 []) =
        let var, mean = varianceAndMeanf32 w
        let stdev = sqrt (var + 1e-5f)
        [| for x in w -> (x - mean) / stdev |]
          
    let inline exponentialAverage f alpha init (data : 'a seq) =
        let y1 =
            defaultArg init (data
                             |> Seq.takeOrMax 5
                             |> Seq.averageBy f)
        data
        |> Seq.fold (fun s_t x ->
               let y = float (f x)
               alpha * y + (1. - alpha) * s_t) y1

    ///low alpha biases towards past, high alpha biases towards most recent
    let inline exponentialSmoothing f alpha eavg point =
        let y = float (f point)
        alpha * y + (1. - alpha) * eavg

    let summaryStats (x : float seq) =
        { Sum = Seq.sum x
          Mean = Seq.average x
          Stddev = stddev x
          N = Seq.length x
          Min = Seq.min x
          Max = Seq.max x
          Median = median (Seq.toArray x) }

    let distCovariance equal (v1 : float []) (v2 : float []) =
        let n = v1.Length
        let nf = float n
        let denum = nf ** 2.
        let distMatrix (v : float []) =
            Array2D.init v.Length v.Length (fun k l -> abs (v.[k] - v.[l]))
        let rowMean (m : float [,]) k = (m |> Array2D.foldAt 1 k (+) 0.) / nf
        let colMean (m : float [,]) l = (m |> Array2D.foldAt 0 l (+) 0.) / nf
        let matrixMean (m : float [,]) = (m |> Array2D.fold (+) 0.) / denum

        let centredDist (v : float []) =
            let distm = distMatrix v
            let meanoverall = matrixMean distm
            let C = Array2D.create n n 0.
            Threading.Tasks.Parallel.For
                (0, n,
                 fun i ->
                     let curRowMean = rowMean distm i
                     for j in 0..n - 1 do
                         C.[i, j] <- distm.[i, j] - curRowMean
                                     - (colMean distm j) + meanoverall)
            |> ignore
            C

        let A, B =
            if equal then
                let A2 = centredDist v1
                A2, A2
            else
                let AB =
                    [| async { return centredDist v1 }
                       async { return centredDist v2 } |]
                    |> Async.Parallel
                    |> Async.RunSynchronously
                AB.[0], AB.[1]

        let _, _, msum =
            A
            |> Array2D.fold (fun (i, j, curSum) value ->
                   let nsum = value * B.[i, j] + curSum
                   if j = n - 1 then i + 1, 0, nsum
                   else i, j + 1, nsum) (0, 0, 0.)

        msum / denum

    ///Distance Correlation is a useful method of calculating correlation with distance covariance that is able to pick up on non-linearities unlike Pearson's. And more flexible than Spearman's rank.
    let distCorrelation v1 v2 =
        let VXY =
            [| async { return distCovariance true v1 v1 }
               async { return distCovariance true v2 v2 } |]
            |> Async.Parallel
            |> Async.RunSynchronously

        let vsig = VXY.[0] * VXY.[1]
        if vsig = 0. then 0.
        else sqrt ((distCovariance false v1 v2) / sqrt vsig)


//***************************PERMUTATIONS AND CHANCE******************// 


let facBigInt n = [ 2I..n ] |> List.reduce (*) 

let permutationsBigInt n k = [ (n - k + 1I)..n ] |> List.reduce (*) 

let combinationsBigInt n k = permutationsBigInt n k / facBigInt k

let fac n = List.reduce (*) [ 2.0..n ]

///num permutations of length n taken k at a time     
let permutations n k = List.reduce (*) [ (n - k + 1.)..n ] 

///num permutations of length n taken k at a time where order does not matter
///let combinations n k = fac n / (fac k * fac (n - k))
let combinations n k = permutations n k / fac k

//This is brilliant! From Harrop's F# for Scientists
let rec powerset s =
    seq { 
        match s with
        | [] -> yield []
        | h :: t -> 
            for x in powerset t do
                yield! [ x
                         h :: x ]
    }

// From: http://stackoverflow.com/questions/286427/calculating-permutations-in-f
let genPermutations collection =
    let rec insertions x =
        function 
        | [] -> [ [ x ] ]
        | (y :: ys) as l -> 
            (x :: l) :: (List.map (fun x -> y :: x) (insertions x ys))
    let rec permutations =
        function 
        | [] -> seq [ [] ]
        | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))
    collection
    |> Seq.toList
    |> permutations
    |> Seq.toList
    
let permutationsTakeN n items =
    genPermutations items
    |> Seq.map (List.take n)
    |> Seq.removeDuplicates

let genCombinations n items =
    permutationsTakeN n items
    |> Seq.map List.sort
    |> Seq.removeDuplicates

type System.Random with
    member t.NextDouble(minim, maxim) = t.NextDouble() * (maxim - minim) + minim

///The X means cross. Cheesy. But stands for thread safe and fast Random Wrapper Class
//http://blogs.msdn.com/b/pfxteam/archive/2009/02/19/9434171.aspx
type RandomX() =
    [<ThreadStatic; DefaultValue>]
    static val mutable private _local : Random
    
    static member checkInst() =
        if RandomX._local = null then 
            let seed = ref 0
            lock (random) (fun () -> seed := random.Next())
            RandomX._local <- new Random(!seed)
        RandomX._local
    
    static member Next(low, high) =
        let x = RandomX.checkInst() //seems a bit faster this way
        x.Next(low, high)
    
    static member Next(max_) =
        let x = RandomX.checkInst()
        x.Next(max_)
    
    static member Next() =
        let x = RandomX.checkInst()
        x.Next()
    
    static member NextDouble() =
        let x = RandomX.checkInst()
        x.NextDouble()
    
    static member NextDouble(minim, maxim) =
        let x = RandomX.checkInst()
        x.NextDouble(minim, maxim)

///Allows one to generate a stream of unique numbers. It is very inefficient if the required set of numbers
///is near the range of the random source.
type randomInt(rNext, lower, upper) =
    let visited = Collections.Generic.HashSet<int>()
    
    member this.stream() =
        seq { 
            while true do
                yield rNext (lower, upper)
        }
    
    member this.resetVisited() = visited.Clear()
    
    member this.uniqueStream() =
        seq { 
            while visited.Count < upper - lower do
                let next =
                    this.stream()
                    |> Seq.skipWhile (visited.Contains)
                    |> Seq.head
                visited.Add next |> ignore
                yield next
        }
    
    member this.nextunique() = this.uniqueStream() |> Seq.head
    member this.next() = this.stream() |> Seq.head

let randompar = new Random()

//***************************ROUNDING******************//
let (|RoundTo|) n x = round n x

///buckets rounds a number to roundTo places then buckets with bucketSize. where all numbers are floored to last 
///multiple of bucketSize so for example
///bucketRange 2 5 n will round to 2 places and allow only multiples of 5. n = 11..14 -> 10
///and n = 15..19 -> 15. for 2 0.5 n, n = 21.0..21.4 -> 21 and n = 21.5..21.9 go to 21.5, integers are unaffected 
let inline bucketRange roundTo bucketSize m =
    let num = abs (round roundTo m)
    let s = float (sign m)
    if num < bucketSize then 0.
    else s * (num - num % bucketSize)

///scaleTo scales a number to rmin and rmax where rangemin and max are the expected range of the number.
///examples: scaleTo -10 10 -1000. 1000. -10. = -0.1, a number with range
///-1000 to 1000 is fit to -10 by 10, scaleTo 0 10 0 100 50 = 5.
let scaleTo rmin rmax rangemin rangemax value =
    let adjrmin, adjrmax, adjval =
        if rangemin < 0. then 0., -rangemin + rangemax, -rangemin + value
        else rangemin, rangemax, value //translate to 0
    (adjval - adjrmin) / (adjrmax - adjrmin) * (rmax - rmin) + rmin

///If |x| < 1, this allows r number of nonzero digits to appear. roundSig 1 0.0000123 = 0.00001
let roundSig r x =
    if abs x < 1. then 
        let p = (log10 x) |> abs |> ceil
        let pten = 10. ** p
        let x' = x * pten
        (round (max 0 (r-1)) x')/pten
    else round r x

///a bit like a bucket, a bit like a round numbers can be flattened to powers of 10.
///10..10..100, 1000...1000...10000, 1000..100..10000 and so on
///n controls how many places to "round" by e.g. where to bucket x * 10^3 by 100 or 10 or 1
let log10bucket n x_ =
    if x_ = 0. then 0.
    elif abs x_ < 1. then x_
    else 
        let x = x_ |> abs |> round 0
        
        let exponent = x |> log10 |> floor
        
        let y_decim = (x * 1. / (10. ** exponent))
        let y' = y_decim |> round n
        y' * (10. ** exponent) * (sign x_ |> float)

let logistic x = 1. / (1. + exp -x)

let logisticRange low high x =
    let x' = scaleTo -6. 6. low high x
    logistic x'

//***************************Array Useful Vector Math******************// 
let inline (|ToFloatArray|) d = d |> Array.map float

module List =
    let shuffle (items : 'a list) =
        items |> List.sortBy (fun _ -> random.Next())

module Array =
    let inline opInPlaceIntoFirst operator (a1 : 'a []) (a2 : 'b []) =
        for i in 0..a1.Length - 1 do
            a1.[i] <- a1.[i] </ operator /> a2.[i]
    
    let inline addInPlaceIntoFirst (a1 : 'a []) (a2 : 'a []) =
        for i in 0..a1.Length - 1 do
            a1.[i] <- a1.[i] + a2.[i]
    
    let inline lp_norm f (vec1 : 'a []) (vec2 : 'a []) =
        let mutable sum = 0.
        for i in 0..vec1.Length - 1 do
            sum <- f (vec1.[i] - vec2.[i]) + sum
        sum

    let inline euclideanDist v v2 = lp_norm (float >> squared) v v2 |> sqrt

    let inline manhattanDist v v2 = lp_norm (float >> abs) v v2
      
    let inline normalizeWeights (a : ('a * 'b) []) =
        let tot = Array.sumBy snd a
        Array.map (keepLeft (flip (/) tot)) a
    
    let inline normalizeInPlace data =
        let total = data |> Array.sum
        data |> Array.iteri (fun i x -> data.[i] <- x / total)
    
    let inline colAverageGen (numRows : 'b) (f : 'a -> 'b) 
               (rows : 'a [] []) =
        let den = numRows
        let outArr = Array.create rows.[0].Length (numRows - numRows) //zero
        for i in 0..rows.[0].Length - 1 do
            for j in 0..int den - 1 do
                outArr.[i] <- outArr.[i] + f rows.[j].[i] / den
        outArr

    let inline shuffle (items : 'a []) =
        items |> Array.sortBy (fun _ -> random.Next())

    let inline Op operator a b = Array.map2 operator a b //NOTE: in defaultof<>,no performance penalty

    let inline dotproduct (v1:_[]) (v2:_[]) =
        let mutable sum = Unchecked.defaultof<'a> 
        for i in 0..v1.Length - 1 do
            sum <- v1.[i] * v2.[i] + sum     
        sum

    let inline magnitude v = sqrt (dotproduct v v)
    
    let inline to_unitvector v =
        let mag = magnitude v
        if mag = Unchecked.defaultof<'a> then v
        else v |> Array.map (flip (/) mag)
    
    let inline normalize (data : ^a []) =
        let total = data |> Array.sum
        data |> Array.map (flip (/) total)
    
    let sampleOne (a : 'a []) = a.[randompar.Next(a.Length)]
    
    let inline normalizeBy f (data : ^a []) =
        let total = data |> Array.sumBy f
        data |> Array.map (f >> flip (/) total)
    
    let inline cosineSimilarityMag tol v1 v2 mag1 mag2 =
        dotproduct v1 v2 / ((mag1 * mag2) + tol)
    
    let transposeJagged (a : 'a [] []) =
        [| for c in 0..a.[0].Length - 1 -> 
               [| for r in 0..a.Length - 1 -> a.[r].[c] |] |]
    
    let inline cosineSimilarity tol v1 v2 =
        dotproduct v1 v2 
        / ((magnitude v1 * magnitude v2) + tol)

    let inline colAverageFloats (rows : 'a [] []) =
        colAverageGen (float rows.Length) float rows


///O(n) in place  
type ``[]``<'a> with
    member arr.permuteYates() =
        let rec doswaps =
            function 
            | 0 -> ()
            | n -> 
                let randN = random.Next(n + 1)
                Array.swapAtIndex n randN arr
                doswaps (n - 1)
        doswaps (arr.Length - 1)

let toBase b n =
    let logf x = log x / log b
    
    let rec breaknum bs =
        function 
        | x when x <= 0. -> bs
        | x -> 
            let p = x |> logf |> floor
            
            let basen = b ** p
            let leading_digit = floor (x / basen)
            let next = x - (leading_digit * basen)
            breaknum ((p, leading_digit) :: bs) next
    breaknum [] n |> List.toArray

let readoutNum b = Array.map (fun (p, d) -> d * b ** p)

let baseNumToArray size (l : _ []) =
    if l.Length = 0 then Array.zeroCreate size
    else 
        let lmap = Map.ofArray l
        [| for i in 0.0..float size - 1. do
               yield (match Map.tryFind i lmap with
                      | None -> 0.
                      | Some d -> d) |]
        |> Array.rev

let baseNumToString (l : _ []) lmap =
    [| for i in 0.0..(fst l.LastElement) do
           yield (match Map.tryFind i lmap with
                  | None -> "0"
                  | Some d when d < 10. -> string d
                  | Some d when d < 36. -> string (char (d + 55.))
                  | Some d -> string d + "|") |]
    |> Array.rev
    |> Strings.joinToString
