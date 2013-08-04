﻿module Prelude.StringMetrics

open System
open Prelude.Common

let characterPairs (s:string) = 
           if s.Length = 1 then Set.singleton s 
           elif s.Length > 1 then  
             s.ToLower().ToCharArray().[0..s.Length-2] 
                                       |> Array.fold (fun (set:Set<string>,i) c -> set.Add(c.ToString() + s.[i+1].ToString().ToLower()) , i + 1) (Set.empty, 0)
                                       |> fst
           else Set.empty                             

let stringSimilarityDice (w1:string) (w2:string) = 
    let bagOfChars = Array.fold (fun fset word -> Set.union fset (characterPairs word)) Set.empty 
    let s1 = w1.Split([|" "|], StringSplitOptions.RemoveEmptyEntries ) |> bagOfChars
    let s2 = w2.Split([|" "|], StringSplitOptions.RemoveEmptyEntries ) |> bagOfChars
    let densum = float(s1.Count + s2.Count )
    if densum = 0. then 0. else 2. * float((Set.intersect s1 s2).Count) / densum

let LevenshteinDistance (word1:string) (word2:string) = 
    let d = Array2D.init (word1.Length + 1) (word2.Length + 1) (fun i j -> if j = 0 then float i elif i = 0 then float j else 0.)
    d |> Array2D.iteri (fun i j (a:float) -> if i = 0 || j = 0 then ()
                                             else if word1.[i-1] = word2.[j-1] then d.[i,j]  <- d.[i-1,j-1]
                                             else d.[i,j] <- ([d.[i-1, j] + 1. ; d.[i , j - 1] + 1. ;d.[i-1, j-1] + 1. ]
                                                                |> List.min) )
    d.[word1.Length,word2.Length]        

let hamming a (b:'a[]) = a |> Array.fold (fun (sum,i) ax -> (sum + if ax <> b.[i] then 1 else 0), i + 1) (0,0) |> fst

///pads out unequal arrays
let hammingpad (a:string) (b:string) =
    let smaller (a:string) (b:string)  = if a.Length > b.Length then b,a else a,b
    let diff = abs(a.Length - b.Length)
    let small, big = smaller a b
    let sp = small + String(' ', diff) 
    hamming (charArr big) (charArr sp)

let bithamming (a:byte[]) (b:byte[]) =  
    let v = (a.[1..a.Length - 1], b.[1..a.Length - 1]) ||> Array.fold2 (fun s a1 b1 -> a1 ^^^ b1 ^^^ s) (a.[0] ^^^ b.[0])
    let rec z d = function
        | v when v = 0uy -> d 
        | v -> z (d + 1) (v &&& v - 1uy)
    z 0 v
     
let damerauLevenshteinDistance (arr1:'a []) (arr2:'a []) =  
    let wrap j k = if j = k then arr2.Length else j - 1 - k 
    let rec outer (oneback:int[]) (twoback:int[]) s = function
       | i when i = arr1.Length || arr2.Length = 0 -> s
       | i ->  let thisrow = Array.zeroCreate (arr2.Length+1) in thisrow.[thisrow.Length - 1] <- i + 1 
               for j in 0..arr2.Length - 1 do 
                    let delcost, addcost, subcost = oneback.[j] + 1, thisrow.[wrap j 0] + 1,
                                                                oneback.[wrap j 0] + if arr1.[i] <> arr2.[j] then 1 else 0
                    thisrow.[j] <- [delcost; addcost; subcost] |> List.min                    
                    if i > 0 && j > 0 && arr1.[i] = arr2.[j-1] && arr1.[i - 1] = arr2.[j] && arr1.[i] <> arr2.[j] then
                                                    thisrow.[j] <-  min (thisrow.[j]) (twoback.[wrap j 1] + 1)     
               outer thisrow oneback thisrow.[arr2.Length - 1] (i + 1) 
    outer (Array.append (Array.init arr2.Length ((+) 1)) [|0|]) (Array.zeroCreate (arr2.Length+1)) (max arr1.Length arr2.Length) 0
    
let longestCommonSubSeq(word1:string) (word2:string) =  
     let d = Array2D.create (word1.Length + 1) (word2.Length + 1) 0
     d |> Array2D.iteri (fun i j a -> if i = 0 || j = 0 then ()
                                      else if word1.[i-1] = word2.[j-1] then d.[i,j]  <- d.[i-1,j-1] + 1
                                      else d.[i,j] <- max d.[i, j-1] d.[i-1 , j])
     d.[word1.Length,word2.Length], word1.Length, word2.Length, d

let rec backtrackLCS(lcsMatrix:int[,]) (str1:string) (str2:string) i j = 
    if i = 0 || j = 0 then  ""
    elif  str1.[i - 1] = str2.[j - 1] then
         backtrackLCS lcsMatrix str1 str2 (i-1) (j-1) + string str1.[i - 1]
    else
        if lcsMatrix.[i,j-1] > lcsMatrix.[i-1,j] then
             backtrackLCS lcsMatrix  str1 str2 i (j-1)
        else
             backtrackLCS lcsMatrix str1 str2 (i-1) j

let longestCommonSubstring (str1:string) (str2: string) =
    let L = Array2D.create str1.Length str2.Length 0
    let mutable z = 0
    let mutable ret = set []
    let m,n = str1.Length - 1, str2.Length - 1
    for i in 0..m do
        for j in 0..n do
            if str1.[i] = str2.[j] then
                if i = 0 || j = 0 then
                    L.[i,j] <- 1
                else
                    L.[i,j] <- L.[i-1,j-1] + 1
                if L.[i,j] > z then
                    z <- L.[i,j]
                    ret <- set ( [str1.[i-z+1..i]])
                if L.[i,j] = z then
                    ret <- ret.Add( str1.[i-z+1..i ] )
            else L.[i,j]<- 0
    ret    
    
//----------MINHASHING--------------
  
//splits like "abcd" -> "ab" "cd"
let inline internal splits hashfunc f op c (data:'a []) =   
  data
    |> Array.fold (fun (bset, curCombo, i) curbit ->
                if i = c then 
                    bset |> Set.add (hashfunc curCombo), f curbit, 1
                else bset, curCombo </op/> f curbit, i + 1) (Set.empty ,f data.[0], 1)
    |> fst3

let internal minhash vset = vset |> Set.minElement  

let minhashes maxiter fset = 
 let rec gethashes count minset fullset = 
  match count with
   | i when i >= maxiter  || Set.count fullset = 0 -> minset
   | i -> let minim = minhash fullset
          gethashes (i + 1) (Set.add minim minset) (Set.remove minim fullset) 
 gethashes 0 Set.empty fset

let strTominHash splitWith k = ((splitWith (+) k) >> (minhashes 5))

let minHashStrDist k str1 str2 = 
    if str1 = "" || str2 = "" then max str1.Length str2.Length 
    else let s1, s2 = strTominHash (splits jenkinsOAThash string) k (charArr str1), 
                      strTominHash (splits jenkinsOAThash string) k (charArr str2)
         (Set.intersect s1 s2).Count 

let minHashStrDistSplitWith splitter k str1 str2 = 
    if str1 = "" || str2 = "" then max str1.Length str2.Length 
    else let s1, s2 = strTominHash splitter k str1, strTominHash splitter k str2
         (Set.intersect s1 s2).Count 