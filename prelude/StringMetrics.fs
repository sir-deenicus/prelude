﻿module Prelude.StringMetrics

open System
open Prelude.Common
open Strings

let inline bithamming zero one (a : ^a) (b : ^a) =
    let rec countbits dist =
        function 
        | v when v = zero -> dist
        | v -> countbits (dist + 1) (v &&& v - one)
    countbits 0 (a ^^^ b)

let bithammingu64 = bithamming 0UL 1UL
let bithammingu32 = bithamming 0ul 1ul
let bithammingByte = bithamming 0uy 1uy

/////////////////////STRING METRICS////////////////

module String =
    let slidingWindowPairs (s: string) =
        [| for (c1, c2) in s.ToCharArray() |> Array.pairwise -> string c1 + string c2 |]

    let chunkedPairs (s : string) =
        [| for (c1, c2) in Array.pairNexts (s.ToCharArray()) do
               yield string c1 + string c2
           if s.Length % 2 <> 0 then yield string s.[s.Length - 1] |]

let characterPairs (s : string) =
    if s.Length = 1 then Set.singleton s
    elif s.Length > 1 then 
        s.ToLower().ToCharArray().[0..s.Length - 2]
        |> Array.fold 
               (fun (set : Set<string>, i) c -> 
               set.Add(c.ToString() + s.[i + 1].ToString().ToLower()), i + 1) 
               (Set.empty, 0)
        |> fst
    else Set.empty


let stringSimilarityDice (w1 : string) (w2 : string) =
    let bagOfChars =
        Array.fold (fun fset word -> Set.union fset (characterPairs word)) 
            Set.empty
    let s1 =
        w1.Split([| " " |], StringSplitOptions.RemoveEmptyEntries) |> bagOfChars
    let s2 =
        w2.Split([| " " |], StringSplitOptions.RemoveEmptyEntries) |> bagOfChars
    let densum = float (s1.Count + s2.Count)
    if densum = 0. then 0.
    else 2. * float ((Set.intersect s1 s2).Count) / densum

let LevenshteinDistance (word1 : string) (word2 : string) =
    let table =
        Array2D.init (word1.Length + 1) (word2.Length + 1) (fun i j -> 
            if j = 0 then float i
            elif i = 0 then float j
            else 0.)
    table
    |> Array2D.iteri (fun i j (a : float) -> 
           if i = 0 || j = 0 then ()
           else if word1.[i - 1] = word2.[j - 1] then 
               table.[i, j] <- table.[i - 1, j - 1]
           else 
               table.[i, j] <- ([ table.[i - 1, j] + 1.
                                  table.[i, j - 1] + 1.
                                  table.[i - 1, j - 1] + 1. ]
                            |> List.min))
    table.[word1.Length, word2.Length]

let hamming a (b : 'a []) =
    a
    |> Array.fold (fun (sum, i) ax -> 
           (sum + if ax <> b.[i] then 1
                  else 0), i + 1) (0, 0)
    |> fst

let internal smaller (a : string) (b : string) =
    if a.Length > b.Length then b, a
    else a, b

///pads out unequal arrays
let hammingpad (a : string) (b : string) =
    let diff = abs (a.Length - b.Length)
    let small, big = smaller a b
    let sp = small + String(' ', diff)
    hamming (char_array big) (char_array sp)


let damerauLevenshteinDistance (arr1: 'a []) (arr2: 'a []) =
    let wrap j k = if j = k then arr2.Length else j - 1 - k

    let rec outer (oneback: int []) (twoback: int []) s =
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
                if i > 0
                   && j > 0
                   && arr1.[i] = arr2.[j - 1]
                   && arr1.[i - 1] = arr2.[j]
                   && arr1.[i] <> arr2.[j] then
                    thisrow.[j] <- min (thisrow.[j]) (twoback.[wrap j 1] + 1)
            outer thisrow oneback thisrow.[arr2.Length - 1] (i + 1)

    outer
        (Array.append (Array.init arr2.Length ((+) 1)) [| 0 |])
        (Array.zeroCreate (arr2.Length + 1))
        (max arr1.Length arr2.Length)
        0

/////////////////////////////////SEQUENCES//////////////////////////
let longestCommonSubSeq (seq1 : 'a seq) (seq2 : 'a seq) =
    let vector1, vector2 = Seq.toArray seq1, Seq.toArray seq2
    let table = Array2D.create (vector1.Length + 1) (vector2.Length + 1) 0
    table
    |> Array2D.iteri (fun i j a -> 
           if i = 0 || j = 0 then ()
           else if vector1.[i - 1] = vector2.[j - 1] then 
               table.[i, j] <- table.[i - 1, j - 1] + 1
           else table.[i, j] <- max table.[i, j - 1] table.[i - 1, j])
    table.[vector1.Length, vector2.Length], 
    (vector1, vector2, table, vector1.Length, vector2.Length)

///reads out the the data generated by longestCommonSubSeq
let rec backtrackLCS (empty : 'b) join lift combine 
        ((vec1 : 'a []), (vec2 : 'a []), (lcsMatrix : int [,]), len1, len2) =
    if len1 = 0 || len2 = 0 then empty
    elif vec1.[len1 - 1] = vec2.[len2 - 1] then 
        (join 
             (backtrackLCS empty join lift combine 
                  (vec1, vec2, lcsMatrix, (len1 - 1), (len2 - 1)))) 
        </ combine /> (lift vec1.[len1 - 1])
    else if lcsMatrix.[len1, len2 - 1] > lcsMatrix.[len1 - 1, len2] then 
        backtrackLCS empty join lift combine 
            (vec1, vec2, lcsMatrix, len1, (len2 - 1))
    else 
        backtrackLCS empty join lift combine 
            (vec1, vec2, lcsMatrix, (len1 - 1), len2)

let inline backtrackLCS_str table =
    backtrackLCS "" joinToString string (+) table

///airie fragile -> aie       
let longestCommonSubSeqStr (word1 : string) (word2 : string) =
    let table = Array2D.create (word1.Length + 1) (word2.Length + 1) 0
    table
    |> Array2D.iteri (fun i j a -> 
           if i = 0 || j = 0 then ()
           else if word1.[i - 1] = word2.[j - 1] then 
               table.[i, j] <- table.[i - 1, j - 1] + 1
           else table.[i, j] <- max table.[i, j - 1] table.[i - 1, j])
    table.[word1.Length, word2.Length], 
    (word1, word2, table, word1.Length, word2.Length)

///reads out the the data generated by longestCommonSuqSeqStr
let backtrackLCStr ((str1 : string), (str2 : string), (lcsMatrix : int [,]), wordlen1, wordlen2) =
    let rec backtrack cont (wordlen1, wordlen2) = 
        if wordlen1 = 0 || wordlen2 = 0 then cont ""
        elif str1.[wordlen1 - 1] = str2.[wordlen2 - 1] then   
            backtrack (fun s -> cont(s + string str1.[wordlen1 - 1])) ((wordlen1 - 1), (wordlen2 - 1))
        else if lcsMatrix.[wordlen1, wordlen2 - 1] > lcsMatrix.[wordlen1 - 1, wordlen2] then  
                cont (backtrack id (wordlen1, (wordlen2 - 1)))
             else cont (backtrack id ((wordlen1 - 1), wordlen2))
    backtrack id (wordlen1,wordlen2)

///apple, applet -> app
let longestCommonSubstring (str1 : string) (str2 : string) =
    let L = Array2D.create str1.Length str2.Length 0
    let mutable z = 0
    let mutable ret = set []
    let m, n = str1.Length - 1, str2.Length - 1
    for i in 0..m do
        for j in 0..n do
            if str1.[i] = str2.[j] then 
                if i = 0 || j = 0 then L.[i, j] <- 1
                else L.[i, j] <- L.[i - 1, j - 1] + 1
                if L.[i, j] > z then 
                    z <- L.[i, j]
                    ret <- set ([ str1.[i - z + 1..i] ])
                if L.[i, j] = z then ret <- ret.Add(str1.[i - z + 1..i])
            else L.[i, j] <- 0
    ret

let longestCommonSubvec (vec1 : 'a []) (vec2 : 'a []) =
    let L = Array2D.create vec1.Length vec2.Length 0
    let mutable z = 0
    let mutable ret = set []
    let m, n = vec1.Length - 1, vec2.Length - 1
    for i in 0..m do
        for j in 0..n do
            if vec1.[i] = vec2.[j] then 
                if i = 0 || j = 0 then L.[i, j] <- 1
                else L.[i, j] <- L.[i - 1, j - 1] + 1
                if L.[i, j] > z then 
                    z <- L.[i, j]
                    ret <- set ([ vec1.[i - z + 1..i] ])
                if L.[i, j] = z then ret <- ret.Add(vec1.[i - z + 1..i])
            else L.[i, j] <- 0
    ret 

///splits like "abcdef" -> "ab" "cd" "ef"
let inline splitNatATimeStr N (str : string) =
    let chars = str.ToCharArray()
    let bset = Hashset()
    let mutable i = 0
    let curCombo = System.Text.StringBuilder()
    for c in 0..chars.Length - 1 do
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
    set bset

