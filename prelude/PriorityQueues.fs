module Prelude.Collections.PriorityQueues

// based on pairing heap published by Okasaki
// original implementation taken from http://lepensemoi.free.fr/index.php/2009/12/17/pairing-heap
//J.F. modified

//from FSharpx.Collections

open System.Collections
open System.Collections.Generic

type IPriorityQueue<'T when 'T : comparison> =
    inherit System.Collections.IEnumerable
    inherit System.Collections.Generic.IEnumerable<'T>

    ///returns true if the queue has no elements
    abstract member IsEmpty : bool with get

    ///returns a new queue with the element added to the end
    abstract member Insert : 'T -> IPriorityQueue<'T>

    ///returns option first element
    abstract member TryPeek : unit -> 'T option

    ///returns the first element
    abstract member Peek : unit -> 'T

    //returns the option first element and tail
    abstract member TryPop : unit -> ('T * IPriorityQueue<'T>) option

    ///returns the first element and tail
    abstract member Pop : unit -> 'T * IPriorityQueue<'T> 

type Heap<'T when 'T : comparison>(isDescending : bool, length : int, data : HeapData<'T> ) =
    let mutable hashCode = None
    member internal this.heapData = data
    member internal this.heapLength = length

    override this.GetHashCode() =
        match hashCode with
        | None ->
            let mutable hash = 1
            for x in this do
                hash <- 31 * hash + Unchecked.hash x
            hashCode <- Some hash
            hash
        | Some hash -> hash

    override this.Equals(other) =
        match other with
        | :? Heap<'T> as y -> 
            if this.Length <> y.Length then false 
            else
                if this.GetHashCode() <> y.GetHashCode() then false
                else Seq.forall2 (Unchecked.equals) this y
        | _ -> false

    static member private merge isDescending newLength (h1 : HeapData<'T>) (h2 : HeapData<'T>) : Heap<'T> = 
        match h1, h2 with
        | E, h -> Heap(isDescending, newLength, h)
        | h, E -> Heap(isDescending, newLength, h)
        | T(x, xs), T(y, ys) ->
            if isDescending then
                if x <= y then Heap(isDescending, newLength, T(y, h1::ys)) else Heap(isDescending, newLength, T(x, h2::xs))
            else 
                if x <= y then Heap(isDescending, newLength, T(x, h2::xs)) else Heap(isDescending, newLength, T(y, h1::ys))

    //http://lorgonblog.wordpress.com/2008/04/06/catamorphisms-part-two
    static member private foldHeap nodeF leafV (h : list<HeapData<'T>>) = 

        let rec loop (h : list<HeapData<'T>>) cont =
            match h with
            | T(a, h')::tl -> loop h'  (fun lacc ->  
                                    loop tl (fun racc -> 
                                    cont (nodeF a lacc racc))) 
            | _ -> cont leafV
        
        loop h (fun x -> x)

    static member private inOrder (h : list<HeapData<'T>>) = (Heap.foldHeap (fun x l r acc -> l (x :: (r acc))) (fun acc -> acc) h) [] 
 
    static member internal ofSeq (isDescending: bool) (s:seq<'T>) : Heap<'T> = 
        if Seq.isEmpty s then Heap(isDescending, 0, E)
        else
            let len, h' =
                 Seq.fold (fun (lnth, (h : 'T HeapData)) x -> 
                    match h with 
                    | E -> 1, T(x, [])
                    | T(y, ys) ->
                    if isDescending then
                        if x <= y then (lnth + 1), T(y, T(x, [])::ys) else (lnth + 1), T(x, T(y, ys)::[])
                    else 
                        if x <= y then (lnth + 1), T(x, T(y, ys)::[]) else (lnth + 1), T(y, T(x, [])::ys) ) (0,E) s
            Heap(isDescending, len, h')
        
    member this.Head = 
        match data with
        | E -> raise (new System.Exception("Heap is empty"))
        | T(x, _) -> x

    member this.TryHead = 
        match data with
        | E -> None
        | T(x, _) -> Some(x)

    member this.Insert x  = 
        Heap.merge isDescending (length + 1) (T(x, [])) data

    member this.IsEmpty = 
        match data with
        | E -> true 
        | _ -> false

    member this.IsDescending = isDescending

    member this.Length = length

    member this.Merge (xs : Heap<'T>) = 
        if isDescending = xs.IsDescending then Heap.merge isDescending (length + xs.heapLength) data xs.heapData
        else failwith "heaps to merge have different sort orders"

    member this.TryMerge (xs : Heap<'T>) = 
        if isDescending = xs.IsDescending then Some(Heap.merge isDescending (length + xs.heapLength) data xs.heapData)
        else None

    member this.Rev() = 
        if isDescending then Heap<'T>.ofSeq false (this :> seq<'T>)
        else  Heap<'T>.ofSeq true (this :> seq<'T>)

    member this.Tail() =

        let mergeData (h1 : HeapData<'T>) (h2 : HeapData<'T>) : HeapData<'T> = 
            match h1, h2 with
            | E, h -> h
            | h, E -> h
            | T(x, xs), T(y, ys) ->
                if isDescending then
                    if x <= y then T(y, h1::ys) else T(x, h2::xs)
                else 
                    if x <= y then T(x, h2::xs) else T(y, h1::ys)

        match data with
        | E -> raise (new System.Exception("Heap is empty"))
        | T(x, xs) -> 
            let combinePairs state item =
                match state with
                | Some p, l ->
                    (None, (mergeData item p)::l)
                | None, l ->
                    (Some item, l)
            
            let tail = 
                xs
                |> List.fold combinePairs (None, [])
                |> function
                   | Some i, l -> i::l
                   | None, l -> l
                |> List.fold mergeData E
            
            Heap(isDescending, (length - 1), tail)
       
    member this.TryTail() =
        match data with
        | E -> None
        | _ -> Some (this.Tail())

    member this.Uncons() = 
        match data with
        | E -> raise (new System.Exception("Heap is empty"))
        | T(x, xs) -> x, this.Tail() 

    member this.TryUncons() = 
        match data with
        | E -> None
        | T(x, xs) -> Some(x, this.Tail())

    interface IEnumerable<'T> with

        member this.GetEnumerator() = 
            let e = 
                let listH = data::[]
                if isDescending
                then Heap.inOrder listH |> List.sort |> List.rev |> List.toSeq
                else Heap.inOrder listH |> List.sort |> List.toSeq

            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> IEnumerator

    interface IPriorityQueue<'T>

        with
        member this.IsEmpty = this.IsEmpty
        member this.Insert element = this.Insert element :> IPriorityQueue<'T>
        member this.TryPeek() = this.TryHead
        member this.Peek() = this.Head

        member this.TryPop() = 
            match this.TryUncons() with
            | Some(element,newHeap) -> Some(element,newHeap  :> IPriorityQueue<'T>)
            | None -> None

        member this.Pop() = 
            let element,newHeap = this.Uncons()
            element,(newHeap  :> IPriorityQueue<'T>)

and HeapData<'T when 'T : comparison> =
    | E 
    | T of 'T * list<HeapData<'T>>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Heap =   
    //pattern discriminator

    let (|Cons|Nil|) (h: Heap<'T>) = match h.TryUncons() with Some(a,b) -> Cons(a,b) | None -> Nil
  
    let empty<'T when 'T : comparison> (isDescending: bool) = Heap<'T>(isDescending, 0, E)

    let inline head (xs: Heap<'T>)  = xs.Head

    let inline tryHead (xs: Heap<'T>)  = xs.TryHead

    let inline insert x (xs: Heap<'T>) = xs.Insert x   

    let inline isEmpty (xs: Heap<'T>) = xs.IsEmpty

    let inline isDescending (xs: Heap<'T>) = xs.IsDescending

    let inline length (xs: Heap<'T>) = xs.Length 

    let inline merge (xs: Heap<'T>) (ys: Heap<'T>) = xs.Merge ys

    let inline tryMerge (xs: Heap<'T>) (ys: Heap<'T>) = xs.TryMerge ys

    let ofSeq isDescending s  = Heap<'T>.ofSeq isDescending s 
    
    let inline rev (xs: Heap<'T>) = xs.Rev()

    let inline tail (xs: Heap<'T>) = xs.Tail()

    let inline tryTail (xs: Heap<'T>) = xs.TryTail()

    let inline toSeq (xs: Heap<'T>) = xs :> seq<'T>

    let inline uncons (xs: Heap<'T>) = xs.Uncons()

    let inline tryUncons (xs: Heap<'T>) = xs.TryUncons()

module ImmutablePriorityQueue =
    ///O(1). Returns a empty queue, with indicated ordering.
    let empty<'T when 'T : comparison> isDescending = Heap.empty isDescending :> IPriorityQueue<'T>

    ///O(1). Returns true if the queue has no elements.
    let inline isEmpty (pq:IPriorityQueue<'T>) = pq.IsEmpty

    ///O(log n) amortized time. Returns a new queue with the element added to the end.
    let inline insert element (pq:IPriorityQueue<'T>) = pq.Insert element

    ///O(1). Returns option first element.
    let inline tryPeek (pq:IPriorityQueue<'T>) = pq.TryPeek()

    ///O(1). Returns the first element.
    let inline peek (pq:IPriorityQueue<'T>) = pq.Peek()

    ///O(log n) amortized time. Returns the option first element and tail.
    let inline tryPop (pq:IPriorityQueue<'T>) = pq.TryPop()

    ///O(log n) amortized time. Returns the first element and tail.
    let inline pop (pq:IPriorityQueue<'T>) = pq.Pop()

////////////////////////////////////////
//Modified from: http://blogs.msdn.com/b/carlnol/archive/2012/05/03/net-implementation-of-a-priority-queue-aka-heap.aspx
type MutablePriorityQueue<'Weight, 'Value when 'Weight: comparison and 'Value: comparison> () =  
    let heapList = List<'Weight*'Value>()
    let positionDict = new Dictionary<'Weight*'Value,int>(HashIdentity.Structural)

    // Determines if a value is in the heap
    let inRange index =
        if index >= 0 && index < heapList.Count then Some(index) else None
   
    let checkIndex index =
        if ((inRange index).IsNone) then failwith(sprintf "Index specified is not within range - %i" index)

    // Gets the children of a node
    let getChildren (index:int) = 
        // children left[2*pos] and right[2*pos + 1] where pos = index + 1
        let left = (2 * index) + 1
        let right = (2 * index) + 2     
        (inRange left, inRange right)

    // Gets the parent of an index
    let getParent (index:int) = 
        // parent index [pos/2] where index = pos - 1
        if (index = 0) then None
        else Some((index-1) / 2)
            
    // Swaps two elements of the heap list
    let swapElements idx1 idx2 = 
        let element1 = heapList.[idx1]
        let element2 = heapList.[idx2]
        heapList.[idx1] <- heapList.[idx2]
        heapList.[idx2] <- element1
        positionDict.[element1] <- idx2
        positionDict.[element2] <- idx1

    // Heapifys toward the parent
    let rec heapifyUp (index:int) =
        if (index > 0) then
            let parent = getParent index
            if (heapList.[parent.Value] > heapList.[index]) then
                swapElements parent.Value index
                heapifyUp parent.Value

    // Heapifys down to the children
    let rec heapifyDown (index:int) = 
        let (left, right) = getChildren index
        if (left.IsSome) then
            let childindex =
                if (right.IsSome && (heapList.[left.Value] > heapList.[right.Value])) then right.Value
                else left.Value
            if (heapList.[index] > heapList.[childindex]) then
                swapElements index childindex
                heapifyDown childindex

    // Heapifys down to the children
    let heapifyUpDown (index:int) = 
        let parent =  getParent index
        if (parent.IsSome && (heapList.[parent.Value] > heapList.[index])) then
            heapifyUp index
        else
            heapifyDown index

    // Adds an items and heapifys
    let insertItem (key) (value) =
        let insertindex = heapList.Count

        if not (positionDict.ContainsKey  (key,value)) then
          positionDict.Add((key,value), insertindex)
          heapList.Add((key, value))
          heapifyUp(insertindex)

    // Delete the root node and heapifys 
    let deleteItem index =
        if (heapList.Count <= 1) then
            heapList.Clear()
            positionDict.Clear()
        else
            let lastindex = heapList.Count - 1
            let indexKey = heapList.[index]
            let lastKey = heapList.[lastindex]
            heapList.[index] <- heapList.[lastindex]
            positionDict.[lastKey] <- index
            heapList.RemoveAt(lastindex)
            positionDict.Remove(indexKey) |> ignore
            heapifyDown index

    // Checks to see if the heap is empty
    member this.IsEmpty
        with get() = (heapList.Count = 0)

    // Enqueues a new entry into the heap
    member this.Enqueue weight value =
        insertItem weight value
         
    // Peeks at the head of the heap
    member this.Peek() = 
        if not(this.IsEmpty) then
            Some heapList.[0]
        else None

    // Dequeues the head entry into the heap
    member this.Dequeue() = 
        match (this.Peek()) with
          | None -> None
          | value -> deleteItem 0
                     value

    // Determines whether an item is in the queue
    member this.Contains key value = 
        heapList.Contains (key, value)

    // Returns the index of a specified item
    member this.IndexOf key value = 
        heapList.IndexOf(key, value)

    // Removes an item from the queue at the specified index
    member this.RemoveAt (index:int) = 
        checkIndex index
        deleteItem index

    // Determines whether an item is in the queue
    member this.ChangeWeight ((weight,item) as current) newWeight =
        let index = positionDict.[current]

        heapList.[index] <- (newWeight, item)
        positionDict.Remove(current) |> ignore
        positionDict.Add((newWeight, item), index)
        heapifyUpDown index
//
//    // Removes an item from the queue for the specified key
    member this.Remove (item) =
        let index = positionDict.[item]
        this.RemoveAt index

    // Returns the count of the queue
    member this.Count
        with get() = heapList.Count

    // Resets the capacity of the Queue
    member this.TrimExcess() = 
        heapList.TrimExcess()   

    // Returns the capacity of the queue
    member this.Capacity 
        with get() = heapList.Capacity

    // Clears the queue
    member this.Clear() = 
        heapList.Clear()   
        positionDict.Clear()

    member this.GetInternalList() = heapList

/////
     