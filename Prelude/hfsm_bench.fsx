#nowarn "1125"

#I @"..\..\DictionarySlim\bin\Release\netstandard2.1"
#r "DictionarySlim.dll"
#r @"bin\Debug\net5\Prelude.dll"

open System
open System.Threading
open Prelude.Common
open Prelude.Control

type BenchState =
    | Ping
    | Pong
    | Parent of ChildState
    | Stop
and ChildState =
    | ChildPing
    | ChildPong

let iterations = 200_000
let samples = 30

let percentile percentileRank (sortedValues: float array) =
    let lastIndex = sortedValues.Length - 1
    let rawIndex = int (ceil (percentileRank * float sortedValues.Length)) - 1
    let index = max 0 (min lastIndex rawIndex)
    sortedValues.[index]

let median (sortedValues: float array) =
    let middle = sortedValues.Length / 2
    if sortedValues.Length % 2 = 0 then
        (sortedValues.[middle - 1] + sortedValues.[middle]) / 2.0
    else
        sortedValues.[middle]

let bench name f =
    let result =
        timeThisWithSetup samples (fun () ->
            GC.Collect()
            GC.WaitForPendingFinalizers()
            GC.Collect()
        ) (fun () ->
            f ())

    let timesMs = result.ElapsedTimes |> List.rev |> List.map (fun span -> span.TotalMilliseconds)
    let sortedTimes = timesMs |> List.sort |> List.toArray
    let avgMs = timesMs |> List.average
    let medianMs = median sortedTimes
    let p95Ms = percentile 0.95 sortedTimes
    let p99Ms = percentile 0.99 sortedTimes
    let minMs = sortedTimes.[0]
    let maxMs = sortedTimes.[sortedTimes.Length - 1]
    name, avgMs, medianMs, p95Ms, p99Ms, minMs, maxMs

let printResult (name, avgMs, medianMs, p95Ms, p99Ms, minMs, maxMs) =
    printfn "%s" name
    printfn "  avg: %.3f ms" avgMs
    printfn "  median: %.3f ms" medianMs
    printfn "  p95: %.3f ms" p95Ms
    printfn "  p99: %.3f ms" p99Ms
    printfn "  min: %.3f ms" minMs
    printfn "  max: %.3f ms" maxMs

let runDirectExact maxCount =
    let rec step state count =
        match state with
        | Ping ->
            if count < maxCount then step Pong (count + 1)
            else count
        | Pong ->
            if count < maxCount then step Ping (count + 1)
            else count
        | _ ->
            count

    step Ping 0

let runDirectHfsm maxCount =
    let rec step state count =
        match state with
        | Parent ChildPing ->
            if count < maxCount then step (Parent ChildPong) (count + 1)
            else count
        | Parent ChildPong ->
            if count < maxCount then step (Parent ChildPing) (count + 1)
            else count
        | _ ->
            count

    step (Parent ChildPing) 0

let runActorExact maxCount =
    use completed = new ManualResetEventSlim(false)
    let mutable finalCount = -1
    let machine = StateMachineExec<BenchState, int>(Stop)

    let ping count =
        if count >= maxCount then
            finalCount <- count
            completed.Set()
            { NextState = Stop; Mem = count }
        else
            { NextState = Pong; Mem = count + 1 }

    let pong count =
        if count >= maxCount then
            finalCount <- count
            completed.Set()
            { NextState = Stop; Mem = count }
        else
            { NextState = Ping; Mem = count + 1 }

    machine.Register(Ping, ping)
    machine.Register(Pong, pong)
    machine.Post({ NextState = Ping; Mem = 0 })
    completed.Wait()
    finalCount

let runActorHfsm maxCount =
    use completed = new ManualResetEventSlim(false)
    let mutable finalCount = -1
    let machine = StateMachineExec<BenchState, int>(Stop)

    let tryMatchParent = function
        | Parent child -> Some child
        | _ -> None

    let stepChild child count =
        if count >= maxCount then
            finalCount <- count
            completed.Set()
            { NextState = Stop; Mem = count }
        else
            let nextChild =
                match child with
                | ChildPing -> ChildPong
                | ChildPong -> ChildPing
            { NextState = Parent nextChild; Mem = count + 1 }

    let matcherId =
        machine.RegisterCase(tryMatchParent, stepChild)

    machine.Post({ NextState = Parent ChildPing; Mem = 0 })
    completed.Wait()
    machine.UnRegisterWhen(matcherId)
    finalCount

let results =
    [ bench (sprintf "Direct exact loop (%d transitions)" iterations) (fun () -> runDirectExact iterations |> ignore)
      bench (sprintf "Direct HFSM loop (%d transitions)" iterations) (fun () -> runDirectHfsm iterations |> ignore)
      bench (sprintf "Actor exact dispatch (%d transitions)" iterations) (fun () -> runActorExact iterations |> ignore)
      bench (sprintf "Actor HFSM dispatch (%d transitions)" iterations) (fun () -> runActorHfsm iterations |> ignore) ]

printfn "HFSM benchmark baseline"
printfn "samples: %d" samples
printfn "iterations per sample: %d" iterations
printfn ""

results |> List.iter printResult