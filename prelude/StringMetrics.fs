module Prelude.StringMetrics

open System
open Prelude.Common

type LcsTrace<'a> = {
    Left: 'a[]
    Right: 'a[]
    Table: int[,]
}

type LongestCommonSubSeqResult<'a> = {
    Length: int
    Trace: LcsTrace<'a>
    Items: 'a[]
}

let inline bithamming zero one (a: ^a) (b: ^a) =
    let rec countbits dist =
        function
        | v when v = zero -> dist
        | v -> countbits (dist + 1) (v &&& v - one)

    countbits 0 (a ^^^ b)

let bithammingu64 = bithamming 0UL 1UL
let bithammingu32 = bithamming 0ul 1ul
let bithammingByte = bithamming 0uy 1uy

module LcsTrace =
    let create left right table =
        {
            Left = left
            Right = right
            Table = table
        }

    let length trace = trace.Table[trace.Left.Length, trace.Right.Length]

    let rec reconstructFrom empty join lift combine trace len1 len2 =
        if len1 = 0 || len2 = 0 then
            empty
        elif trace.Left[len1 - 1] = trace.Right[len2 - 1] then
            (join (reconstructFrom empty join lift combine trace (len1 - 1) (len2 - 1)))
            </ combine /> (lift trace.Left[len1 - 1])
        elif trace.Table[len1, len2 - 1] > trace.Table[len1 - 1, len2] then
            reconstructFrom empty join lift combine trace len1 (len2 - 1)
        else
            reconstructFrom empty join lift combine trace (len1 - 1) len2

    let reconstruct empty join lift combine trace =
        reconstructFrom empty join lift combine trace trace.Left.Length trace.Right.Length

    let toArray trace =
        reconstruct Array.empty id Array.lift Array.append trace

    let toString (trace: LcsTrace<char>) =
        reconstruct "" String.join string (+) trace

module LongestCommonSubSeqResult =
    let create trace =
        {
            Length = LcsTrace.length trace
            Trace = trace
            Items = LcsTrace.toArray trace
        }

    let toString (result: LongestCommonSubSeqResult<char>) =
        LcsTrace.toString result.Trace

/////////////////////STRING METRICS//////////////// 
module Array =
    let hamming (a: 'a[]) (b: 'a[]) =
        //account for size mismatches
        let diff = abs (a.Length - b.Length)
        let smallerLen = min a.Length b.Length

        let rec inner (i: int) (d: int) =
            if i = smallerLen then d + diff
            else if a.[i] = b.[i] then inner (i + 1) d
            else inner (i + 1) (d + 1)

        inner 0 0

    let damerauLevenshteinDistance (arr1: 'a[]) (arr2: 'a[]) =
        let wrap j k =
            if j = k then arr2.Length else j - 1 - k

        let rec outer (oneback: int[]) (twoback: int[]) s =
            function
            | i when i = arr1.Length || arr2.Length = 0 -> s
            | i ->
                let thisrow = Array.zeroCreate (arr2.Length + 1)
                thisrow.[thisrow.Length - 1] <- i + 1

                for j in 0 .. arr2.Length - 1 do
                    let delcost, addcost, subcost =
                        oneback.[j] + 1,
                        thisrow.[wrap j 0] + 1,
                        oneback.[wrap j 0] + if arr1.[i] <> arr2.[j] then 1 else 0

                    thisrow.[j] <- [ delcost; addcost; subcost ] |> List.min

                    if
                        i > 0
                        && j > 0
                        && arr1.[i] = arr2.[j - 1]
                        && arr1.[i - 1] = arr2.[j]
                        && arr1.[i] <> arr2.[j]
                    then
                        thisrow.[j] <- min (thisrow.[j]) (twoback.[wrap j 1] + 1)

                outer thisrow oneback thisrow.[arr2.Length - 1] (i + 1)

        outer
            (Array.append (Array.init arr2.Length ((+) 1)) [| 0 |])
            (Array.zeroCreate (arr2.Length + 1))
            (max arr1.Length arr2.Length)
            0

    let longestCommonSubvec (vec1: 'a[]) (vec2: 'a[]) =
        let L = Array2D.create vec1.Length vec2.Length 0
        let mutable z = 0
        let mutable ret = set []
        let m, n = vec1.Length - 1, vec2.Length - 1

        for i in 0..m do
            for j in 0..n do
                if vec1.[i] = vec2.[j] then
                    if i = 0 || j = 0 then
                        L.[i, j] <- 1
                    else
                        L.[i, j] <- L.[i - 1, j - 1] + 1

                    if L.[i, j] > z then
                        z <- L.[i, j]
                        ret <- set ([ vec1.[i - z + 1 .. i] ])

                    if L.[i, j] = z then
                        ret <- ret.Add(vec1.[i - z + 1 .. i])
                else
                    L.[i, j] <- 0

        ret

/////////////////////////////////SEQUENCES//////////////////////////

module Seq =
    let longestCommonSubSeqTrace (seq1: 'a seq) (seq2: 'a seq) =
        let vector1, vector2 = Seq.toArray seq1, Seq.toArray seq2
        let table = Array2D.create (vector1.Length + 1) (vector2.Length + 1) 0

        table
        |> Array2D.iteri (fun i j a ->
            if i = 0 || j = 0 then
                ()
            else if vector1.[i - 1] = vector2.[j - 1] then
                table.[i, j] <- table.[i - 1, j - 1] + 1
            else
                table.[i, j] <- max table.[i, j - 1] table.[i - 1, j])

        LcsTrace.create vector1 vector2 table

    let readLongestCommonSubSeq trace =
        LcsTrace.toArray trace

    let readLongestCommonSubSeqResult result =
        result.Items

    let longestCommonSubSeqItems (seq1: 'a seq) (seq2: 'a seq) =
        longestCommonSubSeqTrace seq1 seq2
        |> readLongestCommonSubSeq

    let longestCommonSubSeq (seq1: 'a seq) (seq2: 'a seq) =
        let trace = longestCommonSubSeqTrace seq1 seq2
        LongestCommonSubSeqResult.create trace

module String =
    let diceSimilarity (w1: string) (w2: string) =
        let characterPairs (s: string) =
            if s.Length = 1 then
                Set.singleton s
            elif s.Length > 1 then
                s.ToLower().ToCharArray().[0 .. s.Length - 2]
                |> Array.fold
                    (fun (set: Set<string>, i) c -> set.Add(c.ToString() + s.[i + 1].ToString().ToLower()), i + 1)
                    (Set.empty, 0)
                |> fst
            else
                Set.empty

        let bagOfChars =
            Array.fold (fun fset word -> Set.union fset (characterPairs word)) Set.empty

        let s1 = w1.Split([| " " |], StringSplitOptions.RemoveEmptyEntries) |> bagOfChars
        let s2 = w2.Split([| " " |], StringSplitOptions.RemoveEmptyEntries) |> bagOfChars
        let densum = float (s1.Count + s2.Count)

        if densum = 0. then 0.
        else 2. * float ((Set.intersect s1 s2).Count) / densum

    let levenshteinDistance (word1: string) (word2: string) =
        let table =
            Array2D.init (word1.Length + 1) (word2.Length + 1) (fun i j ->
                if j = 0 then float i
                elif i = 0 then float j
                else 0.)

        table
        |> Array2D.iteri (fun i j (a: float) ->
            if i = 0 || j = 0 then
                ()
            else if word1.[i - 1] = word2.[j - 1] then
                table.[i, j] <- table.[i - 1, j - 1]
            else
                table.[i, j] <-
                    ([ table.[i - 1, j] + 1.; table.[i, j - 1] + 1.; table.[i - 1, j - 1] + 1. ]
                    |> List.min))

        table.[word1.Length, word2.Length]

    let damerauLevenshteinDistance (word1: string) (word2: string) = 
        let arr1, arr2 = String.toCharArray word1, String.toCharArray word2
        Array.damerauLevenshteinDistance arr1 arr2
        
    let hammingpad (a: string) (b: string) =
        Array.hamming (String.toCharArray a) (String.toCharArray b)


    let longestCommonSubSeqTrace (word1: string) (word2: string) =
        let table = Array2D.create (word1.Length + 1) (word2.Length + 1) 0

        table
        |> Array2D.iteri (fun i j a ->
            if i = 0 || j = 0 then
                ()
            else if word1.[i - 1] = word2.[j - 1] then
                table.[i, j] <- table.[i - 1, j - 1] + 1
            else
                table.[i, j] <- max table.[i, j - 1] table.[i - 1, j])

        LcsTrace.create (String.toCharArray word1) (String.toCharArray word2) table

    let readLongestCommonSubSeq (trace: LcsTrace<char>) =
        LcsTrace.toString trace

    let readLongestCommonSubSeqResult (result: LongestCommonSubSeqResult<char>) =
        LongestCommonSubSeqResult.toString result

    let longestCommonSubSeqString (word1: string) (word2: string) =
        longestCommonSubSeqTrace word1 word2
        |> readLongestCommonSubSeq

    ///airie fragile -> aie
    let longestCommonSubSeq (word1: string) (word2: string) =
        let trace = longestCommonSubSeqTrace word1 word2
        LongestCommonSubSeqResult.create trace

    ///apple, applet -> app
    let longestCommonSubstring (str1: string) (str2: string) =
        let L = Array2D.create str1.Length str2.Length 0
        let mutable z = 0
        let mutable ret = set []
        let m, n = str1.Length - 1, str2.Length - 1

        for i in 0..m do
            for j in 0..n do
                if str1.[i] = str2.[j] then
                    if i = 0 || j = 0 then
                        L.[i, j] <- 1
                    else
                        L.[i, j] <- L.[i - 1, j - 1] + 1

                    if L.[i, j] > z then
                        z <- L.[i, j]
                        ret <- set ([ str1.[i - z + 1 .. i] ])

                    if L.[i, j] = z then
                        ret <- ret.Add(str1.[i - z + 1 .. i])
                else
                    L.[i, j] <- 0

        ret

    ///splits like "abcdef" -> "ab" "cd" "ef"
    let inline splitNatATime N (str: string) =
        let chars = str.ToCharArray()
        let bset = Hashset()
        let mutable i = 0
        let curCombo = System.Text.StringBuilder()

        for c in 0 .. chars.Length - 1 do
            if i = N then
                bset.Add(jenkinsOAThash (curCombo.ToString())) |> ignore
                i <- 1
                curCombo.Clear() |> ignore
                curCombo.Append chars.[c] |> ignore
            elif c = str.Length - 1 then
                curCombo.Append chars.[c] |> ignore
                bset.Add(jenkinsOAThash (curCombo.ToString())) |> ignore
                curCombo.Clear() |> ignore
                i <- 0
            else
                curCombo.Append chars.[c] |> ignore
                i <- i + 1

        bset
