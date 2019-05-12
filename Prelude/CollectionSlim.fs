﻿module CollectionSlim

//The collections of this module is 100% the code of Anthony Lloyd. 
//Sometimes I might not want to include the Fsion library. 
//I have copy and pasted it here exactly.
//https://github.com/AnthonyLloyd/Fsion/blob/master/Fsion/MapSlim.fs

open System
open System.Runtime.CompilerServices

[<Struct>]
type private Entry<'k,'v> =
    val mutable bucket : int
    val mutable key : 'k
    val mutable value : 'v
    val mutable next : int

type private InitialHolder<'k,'v>() =
    static let initial = Array.zeroCreate<Entry<'k,'v>> 1
    static member inline Initial = initial

///From the Fsion library...https://github.com/AnthonyLloyd/Fsion/blob/master/Fsion/
type MapSlim<'k,'v when 'k : equality and 'k :> IEquatable<'k>> =
    val mutable private count : int
    val mutable private entries : Entry<'k,'v>[]
    new() = {count=0; entries=InitialHolder.Initial}
    new(capacity:int) = {
        count = 0
        entries =
            let inline powerOf2 v =
                if v &&& (v-1) = 0 then v
                else
                    let rec twos i =
                        if i>=v then i
                        else twos (i*2)
                    twos 2
            powerOf2 capacity |> Array.zeroCreate
    }

    member m.Count = m.count

    member private m.Resize() =
        let oldEntries = m.entries
        let entries = Array.zeroCreate<Entry<_,_>> (oldEntries.Length*2)
        for i = oldEntries.Length-1 downto 0 do
            entries.[i].value <- oldEntries.[i].value
            entries.[i].key <- oldEntries.[i].key
            let bi = entries.[i].key.GetHashCode() &&& (entries.Length-1)
            entries.[i].next <- entries.[bi].bucket-1
            entries.[bi].bucket <- i+1
        m.entries <- entries

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    member private m.AddKey(key:'k, hashCode:int) =
        let i = m.count
        if i = 0 && m.entries.Length = 1 then
            m.entries <- Array.zeroCreate 2
        elif i = m.entries.Length then m.Resize()
        let entries = m.entries
        entries.[i].key <- key
        let bucketIndex = hashCode &&& (entries.Length-1)
        entries.[i].next <- entries.[bucketIndex].bucket-1
        entries.[bucketIndex].bucket <- i+1
        m.count <- i+1
        &entries.[i].value

    member m.Set(key:'k, value:'v) =
        let entries = m.entries
        let hashCode = key.GetHashCode()
        let mutable i = entries.[hashCode &&& (entries.Length-1)].bucket-1
        while i >= 0 && not(key.Equals(entries.[i].key)) do
            i <- entries.[i].next
        if i >= 0 then entries.[i].value <- value
        else
            let v = &m.AddKey(key, hashCode)
            v <- value

    member m.GetRef(key:'k) : 'v byref =
        let entries = m.entries
        let hashCode = key.GetHashCode()
        let mutable i = entries.[hashCode &&& (entries.Length-1)].bucket-1
        while i >= 0 && not(key.Equals(entries.[i].key)) do // check >= in IL
            i <- entries.[i].next
        if i >= 0 then &entries.[i].value
        else &m.AddKey(key, hashCode)

    member m.GetRef(key:'k, added: bool outref) : 'v byref =
        let entries = m.entries
        let hashCode = key.GetHashCode()
        let mutable i = entries.[hashCode &&& (entries.Length-1)].bucket-1
        while i >= 0 && not(key.Equals(entries.[i].key)) do
            i <- entries.[i].next
        if i >= 0 then
            added <- false
            &entries.[i].value
        else
            added <- true
            &m.AddKey(key, hashCode)

    member m.GetOption (key:'k) : 'v voption =
        let entries = m.entries
        let mutable i = entries.[key.GetHashCode() &&& (entries.Length-1)].bucket-1
        while i >= 0 && not(key.Equals(entries.[i].key)) do
            i <- entries.[i].next
        if i >= 0 then ValueSome entries.[i].value
        else ValueNone

    member m.Item i : 'k * 'v =
        let entries = m.entries.[i]
        entries.key, entries.value

    member m.Key i : 'k =
        m.entries.[i].key

[<AutoOpen>]
module MapSlimAutoOpen =
    let memoize (f:'a->'b) =
        let d = MapSlim()
        fun a ->
            let mutable isNew = false
            let b = &d.GetRef(a, &isNew)
            if isNew then b <- f a
            b


type ListSlim<'k> =
    val mutable private count : int
    val mutable private entries : 'k[]
    new() = {count=0; entries=Array.empty}
    new(capacity:int) = {count = 0; entries = Array.zeroCreate capacity}

    member m.Count = m.count

    member m.Item
        with get i = m.entries.[i]
        and set i v = m.entries.[i] <- v

    member m.Add(key:'k) =
        let i = m.count
        if i = m.entries.Length then
            if i = 0 then
                m.entries <- Array.zeroCreate 4
            else
                let newEntries = i * 2 |> Array.zeroCreate
                Array.Copy(m.entries, 0, newEntries, 0, i)
                m.entries <- newEntries
        m.entries.[i] <- key
        m.count <- i+1
        i

    member m.ToArray() =
        Array.init m.count (Array.get m.entries)

    member m.ToList() =
        List.init m.count (Array.get m.entries)

[<Struct>]
type private Entry<'k> =
    val mutable bucket : int
    val mutable next : int
    val mutable key : 'k

type private InitialHolder<'k>() =
    static let initial = Array.zeroCreate<Entry<'k>> 1
    static member inline Initial = initial

type SetSlim<'k when 'k : equality and 'k :> IEquatable<'k>> =
    val mutable private count : int
    val mutable private entries : Entry<'k>[]
    new() = {count=0; entries=InitialHolder<_>.Initial}
    new(capacity:int) = {
        count = 0
        entries =
            let inline powerOf2 v =
                if v &&& (v-1) = 0 then v
                else
                    let rec twos i =
                        if i>=v then i
                        else twos (i*2)
                    twos 2
            powerOf2 capacity |> Array.zeroCreate
    }

    member m.Count = m.count

    member private m.Resize() =
        let oldEntries = m.entries
        let entries = Array.zeroCreate<Entry<_>> (oldEntries.Length*2)
        for i = oldEntries.Length-1 downto 0 do
            entries.[i].key <- oldEntries.[i].key
            let bi = entries.[i].key.GetHashCode() &&& (entries.Length-1)
            entries.[i].next <- entries.[bi].bucket-1
            entries.[bi].bucket <- i+1
        m.entries <- entries

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    member private m.AddKey(key:'k, hashCode:int) =
        let i = m.count
        if i = 0 && m.entries.Length = 1 then
            m.entries <- Array.zeroCreate 2
        elif i = m.entries.Length then m.Resize()
        let entries = m.entries
        entries.[i].key <- key
        let bucketIndex = hashCode &&& (entries.Length-1)
        entries.[i].next <- entries.[bucketIndex].bucket-1
        entries.[bucketIndex].bucket <- i+1
        m.count <- i+1
        i

    member m.Add(key:'k) =
        let entries = m.entries
        let hashCode = key.GetHashCode()
        let mutable i = entries.[hashCode &&& (entries.Length-1)].bucket-1
        while i >= 0 && not(key.Equals(entries.[i].key)) do
            i <- entries.[i].next
        if i >= 0 then i
        else m.AddKey(key, hashCode)

    member m.Get(key:'k) =
        let entries = m.entries
        let hashCode = key.GetHashCode()
        let mutable i = entries.[hashCode &&& (entries.Length-1)].bucket-1
        while i >= 0 && not(key.Equals(entries.[i].key)) do
            i <- entries.[i].next
        if i >= 0 then ValueSome i
        else ValueNone

    member m.Item(i) : 'k =
        m.entries.[i].key

    member m.ToArray() =
        Array.init m.count (fun i -> m.entries.[i].key)

    member m.ToList() =
        List.init m.count (fun i -> m.entries.[i].key)