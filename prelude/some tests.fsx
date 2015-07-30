#load "prelude.fs"
#load "pseq.fs"
#load "Reducers.fs"
#load "math.fs"
#load "onlinelearning.fs"
#load "Trie.fs"
#load "TrieStringSearch.fs"
#load "PriorityQueues.fs"
#load "FibonacciHeap.fs"
#load "simplegraph.fs"
#load "stringmetrics.fs"
#time "on"
#nowarn "1125"

open Prelude.Math
open System
open Prelude.Common
open Prelude.Parallel
open Prelude.TrieDictionarySearch 
open System.Net
open Prelude.SimpleGraphs
open Prelude.StringMetrics


Array.collapseCols 
            [|[|"A"; "B"|]
              [|"B"; "C"|]
              [|"A" ; "C"|]|]  |> Array.map Seq.mode 


let x = [|"a", 1; "b", 2|]
let y = [|"a", 1; "b", 3|]

let _x = [|0, 1; 2, 2|]
let _y = [|1, 1; 5, 3|]

addInPlaceIntoFirstGen (fun (_,x) (l,y) -> l, x + y) x y  
x = [|"a",2; "b",5|]
addInPlaceIntoFirstGen addPairs _x _y
_y
_x

let thevec = [|for i in 0..15 -> random.NextDouble(1.,20.)|]
varianceAndMean thevec   
                            
let (v,m,n) = thevec |> Array.fold (fun (v,m,n) x -> online_variance_mean v m n x) (0.,0.,1.)
v/(n-1.), m
/////////////////////////
Array.rot 1 [|1..3|] = [|3;1;2|]
Array.rot 2 [|1..3|] = [|2;3;1|]
Array.rot 3 [|1..3|] = [|1;2;3|]
Array.rot 4 [|1..3|] = Array.rot 1 [|1..3|]  
Array.rot -1 [|1..3|] = Array.rot 2 [|1..3|] 
Array.rot -2 [|1..3|] = Array.rot 1 [|1..3|]
Array.rot -3 [|1..3|] = Array.rot 3 [|1..3|] 
Array.rot -4 [|1..3|] = Array.rot -1 [|1..3|] 
/////////////
splitSentenceManual "ye. water will be 4. dollars.I say\n yes Mr. fred it's .5 to U.C.L.A. and has a Ph.D. And this is a legit sentence too."
//////////////////////

["Apple" ; "BEE"; "CAT"; "Dog"; "elephant"] |> List.map (fun (LowerCase w) -> w)
["Apple" ; "BEE"; "CAT"; "Dog"; "elephant"] |> List.map (fun (UpperCase w) -> w)

///////////////////////
exponentialAverage id 0.45  None [20. ; 10. ; 40. ; 10.; 15.; 20.; 500. ; 500.; 800.]

[20. ; 10. ; 40. ; 10.; 15.; 20.; 500. ; 500.; 800.] |> List.fold (exponentialSmoothing id 0.3) 30. 

///
hoursToText 5.67
hoursToText (25.02)
hoursToText (2. * 168. + 24.)
hoursToText 24.

//////

DateTime.Now.AddDays(-54.).StartOfMonth()
                       

waterfall {
    let! z = Array.tryFind ((=) 2) [|3..5|]
    let! y = Array.tryFind ((=) 1) [|3..5|]
    return 7
  }

waterfall {
    let! z = Array.tryFind ((=) 2) [|3..5|]
    let! y = Array.tryFind ((=) 4) [|3..5|]
    return 7
  }

waterfallOption {
    let! z = Array.tryFind ((=) 2) [|3..5|]
    let! y = Array.tryFind ((=) 1) [|3..5|]
    return 7
  }

waterfallOption {
    let! z = Array.tryFind ((=) 2) [|3..5|]
    let! y = Array.tryFind ((=) 1) [|3..5|]
    ()
  }

nestif {
      let! _ = 2 < 10
      let! _ = 5 < 10
      printfn "good"
   }

nestif {
      let! _ = 2 < 10
      let! _ = 11 < 10
      printfn "good"
   }
   
String.findIndexi true (currysnd ((=) 'f')) 4 "fright"
String.findIndexi false (currysnd ((=) 'f')) 4 "fright"
String.findIndexi false (currysnd ((=) 'i')) 4 "fright"
String.findIndexi true (currysnd ((=) 'i')) 4 "fright"

////////////

longestCommonSubstring "apple" "appetitie"
longestCommonSubvec [|1;2;3;2;3|] [|4;2;3;|]   
longestCommonSubstring "airtight" "foghorn" 
longestCommonSubstring "airtight" "failure" 
let lcs, t = longestCommonSubSeqStr "airtight" "foghorn"
backtrackLCStr t  

longestCommonSubSeqStr "airtight" "failure" |> snd |> backtrackLCStr  
      
longestCommonSubSeq [1..9] [2..2..20] |> snd |> backtrackLCS Array.empty id Array.lift Array.append
longestCommonSubSeq "airtight" "failure" |> snd |> backtrackLCS_str

minHashStrDist 2 "kangaroo" "[angaroo" 

let cstr = splitNatATime id string (+) 2 (charArr "cattarang")
cstr |> Set.map jenkinsOAThash = splitNatATimeStr 2 "cattarang"

strTominHash (splitNatATimeStr 2) "fold"

minHashStrDist 2 "bold" "oldesky"
////
let fg = FastStringGraph()
 
fg.InsertVertex "a"
fg.ContainsVertex "b"
fg.InsertVertex "b"
fg.ContainsVertex "b"
fg.ContainsEdge "a" "b"
fg.InsertEdge ("a", "b")
fg.ContainsEdge "b" "a"
fg.InsertEdge ("a", "c")
fg.InsertVertex "c"
fg.GetEdges "a"
fg.GetEdges "b"
fg.GetEdges "c"
fg.GetEdges "d"

////


///////////

Array.filteriMap (fun i x -> i + 3 < x && x % 2 = 0) squared [|0..2..9|]
[|0..2..9|] |> Array.mapi pair |> Array.filter (fun (i,x) -> i + 3 < x && x % 2 = 0) |> Array.map (snd >> squared)

//Array.mapFilteri squared (fun i x -> i + 3 < x  && x % 2 = 0)  [|0..2..9|]


let z = Array.mapi pair   [|0..2..9|]
///////////
  
"A CHARACTERIZATION OF ENTROPY IN TERMS OF INFORMATION LOSS" |> tolower |> String.capitilizebySpace 2

/////
let teststr0 = "ab"
let teststr1 = "abcdef"

let padtst n f s1 s2 = "\n" + (f n s1) + " efd" + "\n" + (f n s2) + " efd"

padtst 2 String.padcut teststr0 teststr1
teststr1.Length 

let tststr = "This is \"number\" five's test"

tststr.Replace("'", "[apos]").Replace("\"", "[qu]").Replace (newLine, "")
String.replaceMultiple [|"'", "[apos]"; "\"", "[qu]"; newLine,""|] tststr

String.transformMultiple Text.RegularExpressions.Regex.Escape [|"(cat)"; "dog"|] "The animal (cat) slapped the dog"

/////
//= [|2; 3; 7|]
[|[|2; 5; 10|]
  [|1; 2; 3|]
  [|3; 2; 8|]|] |> Array.colAverageFloats 

////
let t = dict_as_trie [|"apple"; "app" ;"art"; "cat"; "card"; "carded"; "cap"|]

autocomplete 2 t "ca" 

//

[1..10] |> Seq.takeOrMax 40 |> Seq.toArray

[1..100] |> filterMapTrunc 3 ((flip (%) 2) >> ((=) 0)) ((*) 2)
  
/////Map Merging

let m1 = Map.ofList [1,2; 2,3]
let m2 = Map.ofList [1,5; 2,2; 3,7; 9,91]

let d1 = Dict.ofIDict m1
let d2 = Dict.ofIDict m2

d1.MergeWith (-) d2
d1.MergeWith konst d2

d1 |> Seq.toArray |> Array.map keyValueToPair

let m3 = Map.merge (+) id m1 m2
let m4 = Map.merge (+) id m2 m1 
m3 = m4 //true
m3 = Map.ofList [1,7; 2,5; 3,7; 9,91]

let m5 = Map.merge (-) id m1 m2
let m6 = Map.merge (-) id m2 m1 
m5 = m6 //false not commutative 
m5 |> Map.map (currysnd abs) = (m6 |> Map.map (currysnd abs))
 
//Folds right?
let m  = Dict.ofSeq [(4,1); (1,3); (2,1)] 

m.foldV (+) 0 //=5

m.fold (fun s k v -> s + k + v) 0 //12

m.fold (fun s k v -> s + string v) "" <> "311"

m.foldKV (fun s (DictKV(k , v)) -> s + k + v) 0 = 12
/////////////
[0..10] |> Seq.takeOrMax 2 |> Seq.length = 2
[0..10] |> Seq.takeOrMax 200 |> Seq.length = 11

///////Testing Reducers /////////
//Example - wordcount

let lines = System.IO.File.ReadAllLines(System.Environment.GetFolderPath(System.Environment.SpecialFolder.MyDocuments) + "\\fhack.txt")
timeThis 100 (fun _ ->
    let c =
        lines
        |> Reducer.toSeqReducer
        |> Reducer.collect (fun line -> Reducer.toSeqReducer <| (line |> splitToWords))
        |> Reducer.countBy id
    c |> Seq.toArray) 
// |> Reducer.groupBy id (fun _ -> 1) (fun (_, items) -> Seq.sum items)

///////////////

strContainsNof 1 [|"apple"; "tree"|] "jumped off the tree"
strContainsNof 2 [|"apple"; "tree"|] "jumped off the tree"
strContainsNof 2 [|"apple"; "tree"|] "jumped off the apple tree"
strContainsNof 2 [|"apple"; "tree"; "plum"|] "jumped off the plum tree"

/////////////
removeExtrasOfString newLine (sprintf "hello%s%sthere%s%s%syes" newLine newLine newLine newLine newLine)

removeExtrasOfString "<br/><br/>" "hey<br/>there<br/><br/>now<br/><br/><br/>oh"

"hey<br/>there<br/><br/>now<br/><br/><br/><br/><br/>oh".splitbystr ("<br/><br/>")  |> joinToStringWith("<br/><br/>")
 

//////
//printfn "%A %A %A" (if n > 0 then s.[n] else ' ') n (found || (lookforward && n = strmax || n = 0 && not lookforward))
            
let (n,[i]) = findSentenceTerminus true 1 0 "bolt the door. quick in here. who goes there."
let (n2,[i2]) = findSentenceTerminus true 1 (i+1) "bolt the door. quick in here. who goes there."
let (n3,[i3]) = findSentenceTerminus true 1 (i2+1) "bolt the door. quick in here. who goes there."

findSentenceTerminus true 2 (i2+1) "bolt the door. quick in here. who goes there."
findSentenceTerminus true 2 (i+1) "bolt the door. quick in here. who goes there."

findSentenceTerminus false 1 0 "bolt the door. quick in here. who goes there."
findSentenceTerminus false 1 5 "bolt the door. quick in here. who goes there."
findSentenceTerminus false 1 (i+1) "bolt the door. quick in here. who goes there."
findSentenceTerminus false 2 (i+1) "bolt the door. quick in here. who goes there."
findSentenceTerminus false 3 (i+1) "bolt the door. quick in here. who goes there."
findSentenceTerminus false 3 (44) "bolt the door. quick in here. who goes there."
findSentenceTerminus false 2 44 "bolt the door. quick in here. who goes there."     

"bolt the door. quick in here. who goes there.".[..i]
"bolt the door. quick in here. who goes there.".[i+1..i2]
"bolt the door. quick in here. who goes there.".[i2+1..i3]
////////////////Testing Threadsafe random numbers
let inf x =
    let var, mean= x |> varianceAndMean
    let minx,maxX = x |> Array.min, x |> Array.max 
    var, mean, minx, maxX

let a, t = timeThis 1 (fun _ -> Threading.Tasks.Parallel.For(0, 50000000, (fun  _ -> RandomX.Next() |> ignore)))

let numParallel = [|0..20000|] |> Array.Parallel.map (fun _ -> RandomX.NextDouble(-100000.,100000.))
let numSeq = [|0..20000|] |> Array.map (fun _ -> random.NextDouble(-100000.,100000.))

let datPar = numParallel |> Array.collect BitConverter.GetBytes
let datSeq = numSeq |> Array.collect BitConverter.GetBytes

let h = Hashset numParallel
let h2 = Hashset numSeq

h.Count, h2.Count
h.IntersectWith h2
h.Count
//test quality by compressing using 7zip, settings dont matter really, e.g true size 157KB -> 147 KB
//threadsafe and regular compress the same amount. Random data should not be very compressible, that it is would indicate bad statistical propeties.
//Very compressible is very predictable. To show bad behevior make a static 
//random variable in the randomX class rather than use the static 'random' defined globally.
IO.File.WriteAllBytes(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)+"\\testRandomPar.dat",datPar)
IO.File.WriteAllBytes(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)+"\\testRandomSeq.dat",datSeq)
inf numParallel 
inf numSeq

//bucketing
bucketRange 0 5. 2. = 0.
bucketRange 0 5. 40. = 40.
bucketRange 1 0.5 1.6 = 1.5

/////contain
strContainsAll [|"apple"; "bag"; "key"|] "apple bag key" 
strContainsAll [|"apple"; "bag"; "key"|] "applebagkey" 
strContainsAll [|"apple"; "bag"; "key"|] "applbagkey"  

strContainsOneOf [|"apple"; "bag"; "key"|] "apple bag key" 
strContainsOneOf [|"apple"; "bag"; "key"|] "applebagkey" 
strContainsOneOf [|"apple"; "bag"; "key"|] "applbagkey" 
strContainsOneOf [|"apple"; "bag"; "key"|] "applbigkay" 

/////////
// From: http://stackoverflow.com/questions/286427/calculating-permutations-in-f
let rec internal insertions x = function
    | []  -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

let genPermutations collection = 
    let rec permutations  = function
        | []      -> seq [ [] ]
        | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs)) 
    collection |> Seq.toList |> permutations 

let dir = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)

let atrain = IO.File.ReadAllLines( dir + @"\arcene_train.data") |> Array.map (fun s -> s.Split([|" " |], StringSplitOptions.RemoveEmptyEntries)|> Array.map (fun f -> float f)) 
let alabels = IO.File.ReadAllLines( dir + @"\arcene_train.labels") |> Array.map (fun f -> let v = float f in if v = -1. then 0. else v)
let alabels2 = IO.File.ReadAllLines( dir + "\\arcene_train.labels") |> Array.map float

let atest = IO.File.ReadAllLines(dir + "\\arcene_valid.data") |> Array.map (fun s -> s.Split([|" " |], StringSplitOptions.RemoveEmptyEntries)|> Array.map (fun f -> float f)) 
let atstlbl = IO.File.ReadAllLines(dir + "\\arcene_valid.labels") |> Array.map (fun f -> let v = float f in if v = -1. then 0. else v)
let atstlbl2 = IO.File.ReadAllLines(dir + "\\arcene_valid.labels") |> Array.map float

let praw = IO.File.ReadAllLines( dir + @"\pima-indians-diabetes.data") |> Array.map (fun s ->  splitstr [|","|] s |> Array.map (fun f -> float f)) 
let pdata_, ptest_ = praw |> Array.map (fun d -> d.[..d.Length - 2], d.LastElement) |> Array.shuffle |> Array.splitByPercent 0.75  
let pdata, plbl = pdata_ |> Array.unzip 
let ptst, ptlbl = ptest_ |> Array.unzip 

let craw = IO.File.ReadAllLines( dir + @"\concreg.csv") |> Array.map (fun s ->  splitstr [|","|] s |> Array.map (fun f -> float f)) 
let cdata_, ctest_ = craw |> Array.map (fun d -> d.[..d.Length - 2], d.LastElement) |> Array.shuffle |> Array.splitByPercent 0.75  
let cdata, clbl = cdata_ |> Array.unzip 
let ctst, ctlbl = ctest_ |> Array.unzip 
 
open Prelude.Onlinelearning 

let classError w cl data lbls = 
       let wrong = (data,lbls) ||> Array.map2 (fun x y -> if y = (cl x w) then 0. else 1.)  
                               |> Array.sum
       wrong, wrong / float lbls.Length, 1. - wrong/float lbls.Length

let w, e  = linearRegressWH 0.000001 cdata clbl None
rootMeanError ctst ctlbl w

let (c, (w,e)) = iterateLearner 50 (linearRegressWH 0.000001)  cdata clbl 
rootMeanError ctst ctlbl w
 
let w, e  = logisticRegress 1. atrain alabels None
classError w (logisticClassify) atest atstlbl
atest |> Array.map (logisticClassify w)
//----
//////////////////////////////////

let w, e  =  logisticRegress 1.4 pdata plbl None
classError w (logisticClassify) ptst ptlbl 
let ptbias, ptbiaslbls = ptest_ |> Array.filter (snd >> (=) 1.) |> Array.unzip
classError w (logisticClassify) ptbias ptbiaslbls 

ptbias |> Array.map (logisticClassify w)

////////////////

containsOne (set ["much"; "many"]) (["how"; "many"; "two"])
containsOne (set ["much"; "many"]) (["how"; "much"; "two"])
containsOne (set ["much"; "many"]) (["how"; "old"; "two"])