module Prelude.Collections.FibonacciHeap

//https://code.google.com/p/fiblib/downloads/detail?name=fiblib-0.1.tar.gz&can=2&q=

(** The Fibonacci heap implementation ported to F#. 1 hr *)

(** Swap the contents of the given ref cells. *)
let swap_refs x y =
  let temp = !x in
    x := !y;
    y := temp;;

(** In this module, we speak natively in terms of a min-heap, but this is only
    an implementation detail that keeps me consistent everywhere nodes need
    ordering. Ordering is determined by the [Ord] parameter to this functor; thus
    you can get a min- or max-heap depending on that ordering.
*)
             
type fibnode<'a,'b when 'b: comparison>(key,parent,child,mark,deg, data) as this =
  [<DefaultValue>] val mutable key : 'b  
  [<DefaultValue>] val mutable parent : fibnode<'a,'b> option 
  [<DefaultValue>] val mutable child : fibnode<'a,'b> option  

  (* We have to use left and right fields instead of an external
    linked-list library in order to acheive the proper complexity of a Fibonacci
    heap, and to keep the code tidy -- we need to be able to know which info is tied
    with which nodes in the linked list. *)
  [<DefaultValue>] val mutable left : fibnode<'a,'b>  
  [<DefaultValue>] val mutable right : fibnode<'a,'b> 

  [<DefaultValue>] val mutable mark : bool 
  (** Has this node lost a child since the last time it was made the
      child of another node? *)

  [<DefaultValue>] val mutable degree : int 
  (** The length of the child list. *)

  let mutable data : 'a = data
  
  do this.key <- key  
     this.parent <- parent
     this.child <- child  
     this.mark <- mark
     this.degree <- deg 
      
  member this.Data = data

type fibheap<'a,'b when 'b: comparison> = {
  mutable min : fibnode<'a,'b> option;
  (** The current optimal element. *)

  mutable n : int;
  (** The number of elements in the heap. *)

  mutable num_marked : int;
}

(* Ord : KeyOrderType *) //struct  
    (** Fibonacci heap doubly-linked list node data structure. This is pretty much
      lifted (or inferred) from the description in CLRS. Note that with doubly-linked,
      circlar lists (like these) a "list" is referred to by *any* node in the
      list. This each fibonacci heap node can represent the fibonacci heap node list.

      Each heap node has a key (for ordering), a parent list, a child list, the
      left and right elements in its list, a mark, a degree, and the data associated
      with this node.  *)   

exception InternalError;;
exception Todo;;

(** Thrown when an operation requires the {!min} node to be present, but it is
    not. *)
exception Empty;;
exception Key_too_big;;


(* vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv *)
(* Utilities *)

(** Make a node containing given fibonacci heap key & data. *)
let create_fibnode data key =
  let n = 
    fibnode(key, parent = None, child = None, mark = false,deg = 0, data=data)
  n.left <- n
  n.right <- n
  n    
    

let fibnode_new (key) (data) = create_fibnode data key


(** Connects [n] and [n'] so that [n.right == n' && n'.left == n]. *)
let splice (n:fibnode<'a,'b>) (n':fibnode<'a,'b>) =
  let right = n.right in
  let left = n'.left in
    n.right <- n';
    n'.left <- n;
    right.left <- left;
    left.right <- right;;


(** Remove [n] from the dllist of which it is a part, properly linking up the
    nodes surrounding [n], if any. [node] will only point to itself on return. *)
let remove (node:fibnode<'a,'b>) =
  let right = node.right in
  let left = node.left in
    left.right <- right;
    right.left <- left;
    (* Make node only point to itself. *)
    node.right <- node;
    node.left <- node;;              


(** Add [new_fibnode] to the root list of [heap], doing the right there if
    there are no nodes in the root list. *)
let root_list_add heap new_fibnode =
  match heap.min with
  | None -> heap.min <- Some new_fibnode
  | Some min_node -> splice new_fibnode min_node


(** @return [true] if and only if the {!left} and {!right} of this node are
    identical with this node. *)                                 
let is_alone (fibnode:fibnode<'a,'b>) = fibnode.right = fibnode && fibnode.left = fibnode


(** If [node] has any children, add them to the rootlist of [heap]. If not, do
    nothing.

    Postconditions of [root_list_add_immediate_children heap node]:

    Let [node'] be the [node] after execution.

    - [node'.child] == [node.child]
    - Each child in {!child}'s list has no {!parent}. *)    
let remove_child (x:fibnode<'a,'b>) =
  (* Before calling [remove x], we have to make sure the parent's child list
      doesn't start with [x]. Once we select a new head for that list, we can safely
      remove x. *)
  let p = Option.get x.parent in
    if is_alone x then
      p.child <- None
    else
      (match p.child with
      | Some x' ->
          (* if x is the "head" of the child list of its parent p, pick a new
              head. *)
          if x = x' then p.child <- Some x.right
      | None -> failwith "Error?");
  p.degree <- p.degree - 1;
  remove x        
                  
(* ---------------------------------------------------------------------- *)
(* Specific Utilities *)

  (** [remove_child x] removes child [x] from its parent, fixing up the child
    pointer of the parent to the proper value.

    Preconditions:
    - x must have a parent

    Postconditions:
    - x will be alone
    - parent's children will be exactly as before, save without x
    - degree of parent decremented
*)                    
 

module FibHeap = 
  
  (** Fold over a {!fibnode} list, traversing {!left} pointers.

      N.B. It is {b not} safe to modify the structure of the list during
      iteration. Strange things may happen.*)
  let fold_left f init (node:fibnode<'a,'b>) =
    let rec loop acc n =
      if n = node.right then f acc n
      else loop (f acc n) n.left

    loop init node


 (** Convert a {!fibnode} list into an OCaml [list] of {!fibnode}s. *)
  let to_list node = fold_left (fun l d -> d :: l) [] node     

 
  (** Iterate over the {!fibnode} list, allowing structure frobbing during
      iteration. *)
  let safe_iter f n = List.iter f (to_list n) 


  (** Fold over a {!fibnode} list, traversing {!right} pointers.

    N.B. It is {b not} safe to modify the structure of the list during
    iteration. Strange things may happen. *)
  let fold_right f (node:fibnode<'a,'b>) init =
    let rec loop acc n =
      if n = node.left then f n acc
      else loop (f n acc) n.right
        
    loop init node      


  (** Iterate over a fibonacci heap node list. Implemented in terms of
      {!fold_right}, so see the warning for {!fold_right}. *)
  let iter_right f node = fold_right (fun x () -> f x) node ()


  (** Iterate over a fibonacci heap node list. Implemented in terms of
      {!fold_left}, so see the warning for {!fold_left}. *)
  let iter_left f node = fold_left (fun () x -> f x) () node      
 

  (** [iterate_fold start times f base] folds over a range, from [start] to
      [times - 1].

      @param start the first index
      @param times the number of times to execute [f]
      @param base the initial value passed to [f] *)
  let rec iterate_fold start times (f : 'a -> int -> 'a) (origin : 'a) =
    if start >= times
    then origin
    else iterate_fold (start + 1) times f (f origin start)

  let internal root_list_add_immediate_children heap (node:fibnode<'a,'b>) =
    match node.child with
    | None -> ()
    | Some child ->
        safe_iter (fun child -> child.parent <- None) child;
        (* Since all the children are in a doubly-linked, circular list, we just
            add child to the root list and the entire child list is spliced in. *)
        root_list_add heap child  

  (** Consolidation works in two steps. *)
  (* Fibonacci heap consolidation.
  ****************************************************

  * This code will blow your mind. View at your own risk. *)

  let internal max_degree heap =
    let log2 x = log x / log 2. in
    let logphi x = log x / log ((1. + sqrt 5.) / 2.) in
      match heap.n with
        (* This is an InternalError because we shouldn't call this function
            unless heap.n > 0. *)
      | 0 -> failwith ":("
      | n -> int (logphi (float n))


    (** Make [new_child] a child of [parent]; and set the parent pointer of
      [new_child] to [parent]. Increments the parent's degree. Evaluates to the
      [parent] with [new_child] added. *)
  let internal add_child (parent:fibnode<'a,'b>) new_child =
    (match parent.child with
    | None -> parent.child <- Some new_child
    | Some child_list -> splice new_child child_list)
    new_child.parent <- Some parent
    parent.degree <- parent.degree + 1  

 
  (** Remove [new_child] from its list, make [new_child] a new child of
      [parent], let adjust [parent.degree] and [new_child.mark] accordingly. *)
  let internal link new_child parent =
    remove new_child
    add_child parent new_child
    new_child.mark <- false   
     
  let internal consolidate heap =
    let a = Array.create (1 + max_degree heap) None in
      (* This loop builds up entries in the array [a]:

         Postcondition: a.(i) contains the *unique* heap tree that has degree
         [i], if there is one. *)
      safe_iter
        (fun w ->
          let d = ref w.degree in
          let x = ref w in

            (* While there is another tree in the heap of the same degree, link
               them: make one a child of the other, depending on the ordering.

               This will increase the degree of the linked tree that's why we
               loop on [d]. *)
            while Option.isSome a.[!d] do
              (* By construction, if we're here, [a.(!d)] is a tree that has the
                 same degree as [x]. *)
              let it: fibnode<'a,'b> ref = ref (Option.get a.[!d]) in
                (* This swap_refsping always keeps the node with higher degree -- the
                   parent -- in the x ref cell. *)
                (if (!it).key < (!x).key then swap_refs x it);
                link (*child*)!it (*parent*) !x;
                (* x now has its degree increased by one. *)
                a.[!d] <- None;
                incr d;
            done;
             (* make sure !x doesn't connect to any other members of a *)
            remove !x;

            a.[!d] <- Some !x;)
        (Option.get heap.min);

      heap.min <- None;

      (* This loop reorganises the heap according to the array [a]. *)
      Array.iter
        (function
          | None -> ()
          | Some (fibnodei:fibnode<'a,'b>) ->
              assert (fibnodei.left = fibnodei);
              assert (fibnodei.right = fibnodei);
              root_list_add heap fibnodei;

              (* There will always be a {!min} because of the previous
                 [root_list_add]. *)
              if fibnodei.key < (Option.get heap.min).key then
                heap.min <- Some fibnodei)
        a;

  let internal cut heap (x:fibnode<'a,'b>) y =
    assert (Option.get x.parent = y);
    remove_child x;
    x.parent <- None;
    root_list_add heap x;
    x.mark <- false;

  let rec internal cascading_cut heap (y:fibnode<'a,'b>) =
    match y.parent with
    | Some z ->
        if not y.mark then
          y.mark <- true
        else
          (cut heap y z;
           cascading_cut heap z)
    | None -> ()           

  (** {3 Operations } *)

  (**
     ------------------------------------------------------------
     CREATE
     ------------------------------------------------------------
  *)
  let create () = {
    min = None;
    n = 0;
    num_marked = 0;
  } 

  (**
     ------------------------------------------------------------
     SIZE
     ------------------------------------------------------------
  *)
  let rec size heap = heap.n

  (**
     ------------------------------------------------------------
     INSERT
     ------------------------------------------------------------
  *)
  let internal insert heap new_fibnode =
    (* Given a fibdata_node insert it appropriately into the heap.

       This function will set the {!min} to the appropriate value; also {!n}
       gets bumped up by one. *)
      root_list_add heap new_fibnode;

      (* There will be a min due to the root_list_add above.

         If the min's key indicates lower priority, set new_fibdata_node as the
         heap.min. *)
      (* Replace {!min} if necessary. *)
      (if new_fibnode.key < (Option.get heap.min).key then
        heap.min <- Some new_fibnode);

      heap.n <- heap.n + 1

  let insert_data heap data key =
    let new_node = fibnode_new key data 
    insert heap new_node;
    new_node     

  (**
    ------------------------------------------------------------
    DECREASE KEY
    ------------------------------------------------------------
  *)
  let decrease_key heap (node:fibnode<'a,'b>) new_key =
    if new_key > node.key then raise Key_too_big;

    node.key <- new_key;
    (* If the parent has a higher key than the new key, pull up the new key. *)
    (match node.parent with
    | Some y ->
        if node.key < y.key then
          (cut heap node y;
           cascading_cut heap y)
    | None -> ());
    if node.key < (Option.get heap.min).key then
      heap.min <- Some node; 

   (**
     ------------------------------------------------------------
     EXTRACT MIN
     ------------------------------------------------------------
  *)
  let extract_min heap =
    match heap.min with
    | None -> raise Empty
    | Some min_fibnode ->
        root_list_add_immediate_children heap min_fibnode;

        if is_alone min_fibnode then
          heap.min <- None
        else
          (heap.min <- Some min_fibnode.right;
           remove min_fibnode;
           consolidate heap);

        heap.n <- heap.n - 1;
        min_fibnode

  let extract_min_data heap =
    (extract_min heap).Data
  (**
     ------------------------------------------------------------
     DELETE
     ------------------------------------------------------------
  *)
  let delete minval heap node =
    decrease_key heap node minval
    ignore (extract_min heap);  
                   