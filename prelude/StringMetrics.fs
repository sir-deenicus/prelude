module Prelude.StringMetrics

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