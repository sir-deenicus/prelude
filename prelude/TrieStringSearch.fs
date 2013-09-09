module Prelude.TrieDictionarySearch

open Prelude.Common
open Trie

let siterm : iter_module<string * int, char> =
 {
  eos  = fun (s, i) -> i >= s.Length;
  head = fun (s, i) -> s.[i];
  tail = fun (s, i) -> (s, i+1)
 }
  
(* opens a dictionary file to construct a trie *)
let dict_as_trie words = words |> Array.fold (fun trie w -> add siterm trie (w, 0) ()) empty_trie 
 
let trie_contains w trie = mem siterm trie (w, 0)

//Based on http://stevehanov.ca/blog/index.php?id=114
let search maxCost take tri word = 
    let results = System.Collections.Generic.List<string * int>()//We could fold but each would generate a list and append is O(n)
    let taken = ref 1

    let rec searchRecursive node letter currentWord word (previousRow:int[]) maxCost =  
        let len = String.length word
        let currentRow = [|0..len|]
        currentRow.[0] <- previousRow.[0] + 1

        // Build one row for the letter, with a column for each letter in the target word, plus one for the empty string at column 0
        for column = 1 to len do 
            let insertCost = currentRow.[column - 1] + 1
            let deleteCost = previousRow.[column] + 1
            let replaceCost = previousRow.[ column - 1 ] + (if word.[column - 1] <> letter then 1 else 0)    
                 
            currentRow.[column] <- List.min [insertCost; deleteCost; replaceCost]

        // if the last entry in the row indicates the optimal cost is less than the maximum cost, and there is a word in this trie node, then add it.        
        if currentRow.[len] <= maxCost && is_terminal node then
          incr taken; results.Add(currentWord + string letter, currentRow.[len]) 

        // if any entries in the row are less than the maximum cost, then recursively search each branch of the trie
        if !taken <= take && currentRow |> Array.min <= maxCost then
            for (DictKV(cletter,children)) in node_map node do
                searchRecursive children cletter (currentWord + string letter) word currentRow maxCost 
   
    let currentRow = [|0..String.length word|]//  

    // recursively search each branch of the trie 
    for (DictKV(letter,children)) in node_map tri do 
        searchRecursive children letter "" word currentRow maxCost 

    results |> Seq.toList 

///telescopic search continually exapnds its search for candidates and takes a minimum of take. zoom is a list containing the series of expansions. e.g. 2 4 8 would increase the depth ever more. 
let telescopicSearch take zoom tri word = 
  let rec tries lastDist words = function 
    | ([],seen) when seen = 0 -> -1, 0, Set.empty
    | [],seen -> lastDist,seen,words
    | _, seen when seen >= take -> lastDist,seen,words
    | (n::ns,seen) ->  
                       match (search n (take-seen) tri word) with
                         | [] -> tries n words (ns,seen)
                         | found when seen >= take -> n, seen, words 
                         | found -> tries n (Set.union words (set found)) (ns, seen + found.Length)
  tries -1 Set.empty (zoom,0)