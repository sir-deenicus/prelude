//#load "prelude.fs"
//#load "pseq.fs"
//#load "Reducers.fs"
//#load "math.fs"
//#load "onlinelearning.fs"
//#load "Trie.fs"
//#load "TrieStringSearch.fs"
//#load "PriorityQueues.fs"
//#load "FibonacciHeap.fs"
//#load "simplegraph.fs"
//#load "stringmetrics.fs"
#time "on"
#nowarn "1125"

//#r @"bin\Release\net47\System.Memory.dll"
#r @"C:\Users\cybernetic\.nuget\packages\dictionaryslim\1.0.0\lib\netstandard2.1\DictionarySlim.dll"
#r @"bin\Release\netstandard2.1\Prelude.dll"
 
open Prelude.Math
open System
open Prelude.Common
open Prelude.Parallel
open Prelude.TrieDictionarySearch 
open System.Net
open Prelude.SimpleGraphs
open Prelude.StringMetrics

open Prelude.SimpleTrees
open Prelude.Common.Strings
open Prelude.SimpleDirectedGraphs
open Prelude 

let g1 = DirectedGraph<int>()

for i in 0..5 do g1.InsertVertex i

g1.InsertEdge(5,2)
g1.InsertEdge(5,0)
g1.InsertEdge(4,0)
g1.InsertEdge(4,1)
g1.InsertEdge(2,3)
g1.InsertEdge(3,1)

let g2 = DirectedGraph<char>()

for c in 'A'..'F' do g2.InsertVertex c

g2.InsertEdge('A','B')
g2.InsertEdge('A','D') 
g2.InsertEdge('B','C')
g2.InsertEdge('C','D')
g2.InsertEdge('C','E')
g2.InsertEdge('D','E')
g2

let g3 = DirectedGraph<int>()

for v in [5;11;2;7;8;9;3;10] do g3.InsertVertex v

g3.InsertEdge(5,11)
g3.InsertEdge(11,2)
g3.InsertEdge(7,11)
g3.InsertEdge(7,8)
g3.InsertEdge(8,9)
g3.InsertEdge(3,8)
g3.InsertEdge(3,10)
g3.InsertEdge(11,9)
g3.InsertEdge(11,10) 


let gi = CompressedDirectedGraph<int,float, _>(byte) 
for i in 1..15 do
    gi.InsertVertex i
    
for x in 2..15 do
    for y in 2..15 do 
        if x <> y && x % y = 0 then
            gi.InsertEdge(x,y,1.) |> ignore

gi.ComputeReverseIndex()    

let kv = Dict.ofIDict (dict ["A", ResizeArray [1..10]])

for KeyValue(k,sq) in kv do 
    for i in 0..sq.Count - 1 do sq[i] <- sq[i] * 2

kv
let proc = System.Diagnostics.Process.GetProcessesByName("explorer")

for p in proc do p.Kill()
//get current assembly, load dagre-template text file 
let assembly =  System.Reflection.Assembly.LoadFrom("bin/Release/netstandard2.1/Prelude.dll")

let template = assembly.GetManifestResourceStream("Prelude.dagre-template.txt")
//read stream bytes
let buffer = Array.create (int template.Length) 0uy
let templateBytes = template.Read(buffer, 0, buffer.Length)
closeAndDispose template
//convert bytes to string
System.Text.Encoding.UTF8.GetString(buffer, 0, templateBytes)

let page = IO.File.ReadAllText @"C:\Users\cybernetic\Documents\Papers\dagre-template.txt" 
let disptemplate = IO.File.ReadAllText @"C:\Users\cybernetic\Documents\Papers\disp-template.txt"

let disp2(gd:IGraph<string>) =
    let gtxt = GraphVisualization.createDagreGraphU string 30 30 gd
    let fout = disptemplate.Replace ("__TEXT__", GraphVisualization.disp page  true "n1" 1200 1200 gtxt)
    IO.File.WriteAllText(@"C:\Users\cybernetic\Documents\Papers\disp.htm", fout)   

let disp(g0:IWeightedGraph<string,float>) =
    let gtxt = GraphVisualization.createDagreGraph string string 30 30 g0
    let fout = disptemplate.Replace ("__TEXT__", GraphVisualization.disp page  false "n1" 800 500 gtxt)
    IO.File.WriteAllText(@"C:\Users\cybernetic\Documents\Papers\disp.htm", fout)
    

     
let gd = DirectedGraph<string>()
 
gd.InsertVertex "X"; gd.InsertVertex "Y";gd.InsertVertex "Z"; gd.InsertVertex "U"
gd.InsertVertex "A"
gd.InsertVertex "B"
gd.InsertVertex "C"
gd.InsertVertex "D"
gd.InsertVertex "E"
gd.InsertVertex "F"
gd.InsertVertex "G"
gd.InsertEdge("U", "X")
gd.InsertEdge("U", "Y")
gd.InsertEdge("X", "Y")
gd.InsertEdge("Z", "X")
gd.InsertEdge("B", "A")
gd.InsertEdge("C", "B")
gd.InsertEdge("A", "C") 
gd.InsertEdge("D", "B" )
gd.InsertEdge("D", "C" )
gd.InsertEdge("E", "D" )
gd.InsertEdge("C", "E")
gd.InsertEdge("F", "D") 
gd.InsertEdge("E", "F") 
gd.InsertEdge("A", "F") 
gd.InsertEdge("F", "G" )
gd.InsertEdge("E", "G" )
gd.InsertEdge("D", "G" )
gd.InsertEdge("G", "D" )
gd.InsertEdge("F", "F")
gd.InsertEdge("G", "C" )
gd.InsertEdge("G", "Y" )
gd.InsertEdge("B", "G" )
gd.InsertEdge("A", "Z" )
gd.InsertEdge("Y", "D" ) 
 
GraphAlgorithms.removeCycles gd
  

                
Branch("B", [Node "A"; Branch("C", [Node "D"; Branch("E", [Node "E1"; Node "E1b"])]); Node "F"])
|> dispTree id

graphToTree gd "X" |> dispTree id  

disp2 gd

let disconnectedSubGraphs (all:Hashset<_>) (first, g:IGraph<_>) = 
    let seen = Hashset()
    let edges = Hashset()
    let rec build v =
        let inNodes = if g.IsDirected then g.Ins v else Array.empty 
        let outNodes = Option.defaultValue Array.empty (g.GetNeighbors v) 
        seen.Add v |> ignore; all.Remove v |> ignore
        for v2 in inNodes do  
            edges.Add(v2, v) |> ignore
            if not(seen.Contains v2) then build v2
        for v2 in outNodes do 
            edges.Add(v,v2) |> ignore
            if not(seen.Contains v2) then build v2
    build first
    Seq.toArray edges

let ffb (all:Hashset<_>) (first, g:IGraph<_>) = 
    let seen = Hashset()
    let rec build v =
        [|  let inNodes = if g.IsDirected then g.Ins v else Array.empty
                          |> Array.filter (seen.Contains >> not)
            let outNodes = Option.defaultValue Array.empty (g.GetNeighbors v)
                           |> Array.filter (seen.Contains >> not)
            seen.Add v |> ignore; all.Remove v |> ignore
            for v2 in inNodes do  
                yield (v2,v)  
                if not(seen.Contains v2) then yield! build v2
            for v2 in outNodes do 
                yield (v, v2)  
                if not(seen.Contains v2) then yield! build v2|] 
    build first 
    
let ff2 (first, g:IGraph<_>) = 
    let all = Hashset(g.Vertices)
    let rec loop node1 =
        [| yield (ffb all (node1, g)) 
           if all.Count > 0 then yield ffb all (Seq.head all, g)|] 
    defaultArg first (Seq.head all)
    |> loop 
 
let gd2 = DirectedGraph<string>()
ff2 (None, g0) 
gd2.InsertVertex "X"
gd2.InsertVertex "Y"
gd2.InsertVertex "Z"
gd2.InsertVertex "U"
gd2.InsertVertex "A"
gd2.InsertVertex "B"
gd2.InsertVertex "C"
gd2.InsertVertex "D"
gd2.InsertEdge("U", "X")
gd2.InsertEdge("U", "Y")
gd2.InsertEdge("X", "Y")
gd2.InsertEdge("Z", "X")
gd2.InsertEdge("B", "A")
gd2.InsertEdge("C", "B")
gd2.InsertEdge("A", "C") 
gd2.InsertEdge("D", "B" )
 
let first = (Seq.head gd2.Vertices)
let all = Hashset (gd2.Vertices)
Seq.toArray all


for _ in 1..100000 do ffb all (first,gd2)

disp2 gd2



let g0 = CompressedDirectedGraph<string,float,_>(uint16, true)
 
g0.InsertVertex "X"; g0.InsertVertex "Y";g0.InsertVertex "Z"; g0.InsertVertex "U"
g0.InsertEdge("U", "X", 1.)
g0.InsertEdge("U", "Y", 1.)
g0.InsertEdge("X", "Y", 1.)
g0.InsertEdge("Z", "X", 1.)

g0.InsertVertex "A"
g0.InsertVertex "B"
g0.InsertVertex "C"
g0.InsertVertex "D"
g0.InsertVertex "E"
g0.InsertVertex "F"
g0.InsertVertex "G"
g0.InsertEdge("B", "A" , 2.)
g0.InsertEdge("C", "B" , 3.)
g0.InsertEdge("A", "C" , 3.) 
g0.InsertEdge("D", "B" , 1.)
g0.InsertEdge("D", "C" , 1.)
g0.InsertEdge("E", "D" , 1.)
g0.InsertEdge("C", "E" ,5.3)
g0.InsertEdge("F", "D" ,4.) 
g0.InsertEdge("E", "F" ,2.) 
g0.InsertEdge("A", "F" ,6.) 
g0.InsertEdge("F", "G" ,5.2)
g0.InsertEdge("E", "G" ,5.2)
g0.InsertEdge("D", "G" ,5.2)
g0.InsertEdge("G", "D" ,5.2)
g0.InsertEdge("F", "F" ,5.21)
g0.InsertEdge("G", "C" ,5.2)
g0.InsertEdge("G", "Y" ,5.2)
g0.InsertEdge("B", "G",1. )
g0.InsertEdge("A", "Z",1. )
g0.InsertEdge("Y", "D",1. ) 

g0.ComputeReverseIndex()  
disp g0

GraphAlgorithms.removeCycles gd2

disp g0
GraphAlgorithms.GetNeighbors(g0, "F", 4)
|> List.groupBy snd
|> List.mapRight (List.map fst)
    
g0
g0.Edges
g0.ForEachEdge ((+) 1.)

g0.Ins "A"
g0.Ins "C"
g0
disp2 gd2
//let (Ok order) = 
GraphAlgorithms.isCyclic gd2
GraphAlgorithms.topologicalSort gd2

for _ in 1..1_000_000 do GraphAlgorithms.isCyclic g0 |> ignore
 
let (Choice1Of2 tc) = GraphAlgorithms.minimumSpanningTree g0 

let (Ok order) = GraphAlgorithms.topologicalSort gd2 

GraphAlgorithms.shortestPath(gd2.toWeightedGraph(),order,  "U")
|> snd 
|> GraphAlgorithms.readOffPath "Y"   


let t = graphToTree gd ("C")

dispTree id t

let tvs, tes = toVerticesAndEdges ("") t

let d = DirectedGraph<string>()

for (v) in tvs do d.InsertVertex v

for (n1), (n2) in tes do d.InsertEdge(n1,n2)

disp2 d

GraphAlgorithms.isCyclic d
SimpleTrees.treeDepth 0 t
flattenWithShortPathBias t  

weightedGraphToTree g0 ("C", 0.)
//|> find (fst >> (=) "D") 
//|> dispTree string 
|> flattenWithShortPathBias

 
Branch("B", [Node "A"; Branch("C", [Node "D"; Branch("E", [Node "E1"])]); Node "F"])
//|> dispTree id
//|> find ((=) "E")  
|> toVerticesAndEdges ""

(*
let commaNumber (ToString str) = 
    let num,s = if str.[0] = '-' then str.[1..],"-" else str,""
    let decimpoint = let i = num.IndexOf(".") in if i = -1 then num.Length else i
    let npart , decimalpart = num.[..decimpoint-1], (num.[decimpoint..] )

    let num' = Seq.chunkBySize 3 (Seq.rev npart) |> Seq.map (Array.rev >> joinToString) |> Seq.rev |> joinToStringWith ","
    s + num' + (joinToString decimalpart)

*)

open Prelude.Math.Stats

let dat = [2.,6.; 3.,8. ;12.,9.;5.,2.;16.,2.] 

dat |> List.fold online_covariance (0.,0.,0.,0.) 

dat |> List.unzip ||> simpleStats

dat |> List.map fst |> varianceAndMean 
dat |> List.averageBy snd



Array.collapseCols 
    [|  [|"A"; "B"|]
        [|"B"; "C"|]
        [|"A" ; "C"|]|]
    |> Array.map Seq.mode 
 
 
Array.splitEvenly 2 [|1..9|]

let thevec = [|for i in 0..15 -> random.NextDouble(1.,20.)|]
varianceAndMean thevec   
                            
let (v,m,n) = thevec |> Array.fold (fun (v,m,n) x -> online_variance_mean v m n x) (0.,0.,1.)
v/(n-1.), m

///////


timedn 3 {yield 3}

timed { yield 3 } 

timed {for _ in 1..2 -> 3} 

timeds false 3 {for _ in 1..3 -> 3} 

timeds false 0 {for _ in 1..3 -> 3} 

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
                       

errorFall {
   let! e, b = lazy(IO.File.ReadAllBytes <| combinePaths [__SOURCE_DIRECTORY__; "prelude"; "prelude.fs"])
   let! e2, b2 = lazy(IO.File.ReadAllBytes "none") 
   return e,b2
 }

errorFall {
   let! e2, b2 = lazy(IO.File.ReadAllBytes "none")
   let! e, b = lazy(IO.File.ReadAllBytes <| combinePaths [__SOURCE_DIRECTORY__; "prelude"; "prelude.fs"]) 
   return e,b2
 }

errorFall {
   let! e2, b2 = lazy(IO.File.ReadAllBytes "none")                                                         
   return e2,b2
 }


   
String.findIndexi((fun _ c -> c = 'f'), "fright", start = 4, direction = Direction.Forward)
String.findIndexi((fun _ c -> c = 'f'), "fright", start = 4, direction = Direction.Backwards)  

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

let cstr = splitNatATime id string (+) 2 (Strings.char_array "cattarang")
cstr |> Set.map jenkinsOAThash = splitNatATimeStr 2 "cattarang"

strTominHash (splitNatATimeStr 2) "fold"

minHashStrDist 2 "bold" "oldesky"
////

let wg = WeightedGraph<string>()

wg.InsertVertex("a")
wg.InsertVertex("b")
wg.InsertVertex("c")
wg.InsertEdge("a","b", 2.)
wg.InsertEdge("c","b", 2.)

wg.AdjustWeight ((+) 1.) ("a", "b") 

wg
////


///////////

Array.filteriMap (fun i x -> i + 3 < x && x % 2 = 0) squared [|0..2..9|]
[|0..2..9|] |> Array.mapi Tuple.pair |> Array.filter (fun (i,x) -> i + 3 < x && x % 2 = 0) |> Array.map (snd >> squared)

//Array.mapFilteri squared (fun i x -> i + 3 < x  && x % 2 = 0)  [|0..2..9|]


let z = Array.mapi Tuple.pair  [|0..2..9|]
///////////
  
"A CHARACTERIZATION OF ENTROPY IN TERMS OF INFORMATION LOSS" |> tolower |> Strings.capitilizebySpace 2

/////
let teststr0 = "ab"
let teststr1 = "abcdef"

let padtst n f s1 s2 = "\n" + (f n s1) + " efd" + "\n" + (f n s2) + " efd"

padtst 2 Strings.padcut teststr0 teststr1
teststr1.Length 

let tststr = "This is \"number\" five's test"

tststr.Replace("'", "[apos]").Replace("\"", "[qu]").Replace (newLine, "")
Strings.replaceMultiple [|"'", "[apos]"; "\"", "[qu]"; newLine,""|] tststr

Strings.transformMultiple Text.RegularExpressions.Regex.Escape [|"(cat)"; "dog"|] "The animal (cat) slapped the dog"

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

[1..100] |> Seq.filterMapTake 3 ((flip (%) 2) >> ((=) 0)) ((*) 2)
  
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
m5 |> Map.map (fun _ x -> abs x) = (m6 |> Map.map (fun _ x -> abs x))
 
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
time_this 100 id (fun _ ->
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
 


////////////////Testing Threadsafe random numbers
let inf x =
    let var, mean= x |> varianceAndMean
    let minx,maxX = x |> Array.min, x |> Array.max 
    var, mean, minx, maxX

time_this 1 () id (fun _ -> Threading.Tasks.Parallel.For(0, 50000000, (fun  _ -> RandomX.Next() |> ignore)) |> ignore )

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
     

////////////////
open Prelude.Sampling
 
cdf [|"a",0.5;"b",0.2;"c",0.3|]

[| for _ in 0..9999 -> discreteSample [|"a",5.;"b",2.;"c",2.5; "d", 0.5|] |]


open Prelude.Control
//open FSharp.Control.Reactive

let mm = StateMachineExec<string,int>("Q") 

 
let A c = 
    printfn "In A: %A" c
    if c <= 5 then 
        {NextState = "B"; Mem = c + 1}
    else {NextState = "Q"; Mem = c}
 

let B (c) = 
    printfn "In B: %A" c 
    {NextState = "A"; Mem = c + 1}
         

mm.Register("A", A)
mm.Register("B", B)

mm.Post({NextState = "A"; Mem = 0})

//////////////////////

