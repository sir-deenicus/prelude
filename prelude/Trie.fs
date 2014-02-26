module Prelude.Trie
(*
This code is from: http://blog.lab49.com/archives/2841
Only changes made are to bring it up to date to Modern F#
 trie.ml
  Defines a simple trie structure for strings. 
*)
 

type iter_module<'ks, 'k> = { eos : ('ks -> bool); head : ('ks -> 'k); tail : ('ks -> 'ks) }

type trie<'k,'a  when 'k : comparison>  = 
  TNode of ('a option * Map<'k, trie<'k, 'a>>)

(* trie utility functions *)
let node_value = function
 | TNode (ov, _) -> ov 

let node_map = function
 | TNode (_, m) -> m 

let tri_unbundle = function | TNode (k, m) -> k, m
 
let is_terminal n =
 match node_value n with
 | Some _ -> true
 | None   -> false 

let is_empty tn = Map.isEmpty (node_map tn)
let empty_trie  = TNode (None, Map.empty)

let find_subtrie tn k =
 try
  Map.find k (node_map tn)
 with
  not_found -> empty_trie

let path_heads tn = Map.fold (fun ks k _  -> k::ks) [] (node_map tn)

(* add a word to a trie *)
let add m tn ks v =
 let rec upd tn' ks' =
  if (m.eos ks') then
   TNode (Some v, (node_map tn'))
  else
   let k = (m.head ks') in
    TNode (node_value tn', Map.add k (upd (find_subtrie tn' k) (m.tail ks')) (node_map tn'))
 in
  upd tn ks

(* lookup a value in a trie *)
let lookup m tn ks =
 let rec lv tn' ks' =
  if (m.eos ks') then
   node_value tn'
  else
   let k = (m.head ks') in
    lv (find_subtrie tn' k) (m.tail ks')
 in
  lv tn ks

(* determine whether or not a key-sequence exists in a trie *)
let mem m tn ks =
 match (lookup m tn ks) with
 | Some _ -> true
 | None   -> false

(* produces the dot/graphviz description of a trie *)
let graphviz ovstr kstr tn =
 let rec nodes p tn' =
  let scs k = nodes (p + "_" + (kstr k)) (find_subtrie tn' k) in
   let nshape = if is_terminal tn' then "circle" else "point" in
   (" " + p + " [shape=\"" + nshape + "\" label=\"" + (ovstr (node_value tn')) + "\"];\n") +
   (String.concat "" (List.map scs (path_heads tn')))
 in
 let rec connections p tn' =
  let
   scs k =
    let sn = p + "_" + (kstr k) in
    (" " + p + " -> " + sn + " [ label = \"" + (kstr k) + "\"];\n") + (connections sn (find_subtrie tn' k))
  in
   String.concat "" (List.map scs (path_heads tn'))
 in
 "digraph g {\n" + (nodes "root" tn) + (connections "root" tn) + "}\n"

