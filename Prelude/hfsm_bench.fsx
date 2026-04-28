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

type BenchStateKey =
    | PingKey
    | PongKey
    | ParentKey
    | ChildPingKey
    | ChildPongKey
    | StopKey

let benchStateKey = function
    | Ping -> PingKey
    | Pong -> PongKey
    | Parent ChildPing -> ChildPingKey
    | Parent ChildPong -> ChildPongKey
    | Stop -> StopKey

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
    let machine = StateMachineExec<BenchState, int>(Stop)

    let ping count =
        if count >= maxCount then
            { NextState = Stop; Mem = count }
        else
            { NextState = Pong; Mem = count + 1 }

    let pong count =
        if count >= maxCount then
            { NextState = Stop; Mem = count }
        else
            { NextState = Ping; Mem = count + 1 }

    machine.Register(Ping, ping)
    machine.Register(Pong, pong)
    let status = machine.RunOnActor({ NextState = Ping; Mem = 0 })

    match status.StopReason with
    | ReachedExit transition -> transition.Mem
    | stopReason -> invalidOp (sprintf "Expected ReachedExit, got %A" stopReason)

let runActorHfsm maxCount =
    let machine = StateMachineExec<BenchState, int>(Stop)

    let tryMatchParent = function
        | Parent child -> Some child
        | _ -> None

    let finish count =
        { NextState = Stop; Mem = count }

    let stepChild child count =
        if count >= maxCount then
            finish count
        else
            let nextChild =
                match child with
                | ChildPing -> ChildPong
                | ChildPong -> ChildPing
            { NextState = Parent nextChild; Mem = count + 1 }

    let matcherId =
        machine.RegisterCase(tryMatchParent, stepChild)

    let status = machine.RunOnActor({ NextState = Parent ChildPing; Mem = 0 })
    machine.UnRegisterWhen(matcherId)

    match status.StopReason with
    | ReachedExit transition -> transition.Mem
    | stopReason -> invalidOp (sprintf "Expected ReachedExit, got %A" stopReason)

let exitMem status =
    match status.StopReason with
    | ReachedExit transition -> transition.Mem
    | stopReason -> invalidOp (sprintf "Expected ReachedExit, got %A" stopReason)

let createOptimizedExactMachine maxCount =
    let ping count =
        if count >= maxCount then
            { NextState = Stop; Mem = count }
        else
            { NextState = Pong; Mem = count + 1 }

    let pong count =
        if count >= maxCount then
            { NextState = Stop; Mem = count }
        else
            { NextState = Ping; Mem = count + 1 }

    let exactTransitions =
        [ Ping, ping
          Pong, pong ]

    let machine : OptimizedStateMachine<BenchState, BenchStateKey, int> =
        optimizedStateMachine {
            exit_state Stop
            state_key benchStateKey
            exacts exactTransitions
        }

    machine

let createOptimizedHfsmMachine maxCount =
    let stepParent state count =
        match state with
        | Parent child ->
            if count >= maxCount then
                { NextState = Stop; Mem = count }
            else
                let nextChild =
                    match child with
                    | ChildPing -> ChildPong
                    | ChildPong -> ChildPing
                { NextState = Parent nextChild; Mem = count + 1 }
        | _ -> invalidOp (sprintf "Expected Parent state, got %A" state)

    let machine : OptimizedStateMachine<BenchState, BenchStateKey, int> =
        optimizedStateMachine {
            exit_state Stop
            state_key benchStateKey
            children ParentKey [ ChildPingKey; ChildPongKey ]
            hierarchy ParentKey stepParent
        }

    machine

let runOptimizedActorExact maxCount =
    createOptimizedExactMachine maxCount
    |> fun machine -> machine.CreateInstance(startActor = true)
    |> fun instance -> instance.RunOnActor({ NextState = Ping; Mem = 0 })
    |> exitMem

let runOptimizedSyncExact maxCount =
    createOptimizedExactMachine maxCount
    |> fun machine -> machine.CreateInstance()
    |> fun instance -> instance.Run({ NextState = Ping; Mem = 0 })
    |> exitMem

let runOptimizedActorHfsm maxCount =
    createOptimizedHfsmMachine maxCount
    |> fun machine -> machine.CreateInstance(startActor = true)
    |> fun instance -> instance.RunOnActor({ NextState = Parent ChildPing; Mem = 0 })
    |> exitMem

let runOptimizedSyncHfsm maxCount =
    createOptimizedHfsmMachine maxCount
    |> fun machine -> machine.CreateInstance()
    |> fun instance -> instance.Run({ NextState = Parent ChildPing; Mem = 0 })
    |> exitMem

let results =
    [ bench (sprintf "Direct exact loop (%d transitions)" iterations) (fun () -> runDirectExact iterations |> ignore)
      bench (sprintf "Direct HFSM loop (%d transitions)" iterations) (fun () -> runDirectHfsm iterations |> ignore)
      bench (sprintf "Actor exact dispatch (%d transitions)" iterations) (fun () -> runActorExact iterations |> ignore)
      bench (sprintf "Actor HFSM dispatch (%d transitions)" iterations) (fun () -> runActorHfsm iterations |> ignore)
      bench (sprintf "Optimized actor exact dispatch (%d transitions)" iterations) (fun () -> runOptimizedActorExact iterations |> ignore)
      bench (sprintf "Optimized actor HFSM dispatch (%d transitions)" iterations) (fun () -> runOptimizedActorHfsm iterations |> ignore)
      bench (sprintf "Optimized sync exact dispatch (%d transitions)" iterations) (fun () -> runOptimizedSyncExact iterations |> ignore)
      bench (sprintf "Optimized sync HFSM dispatch (%d transitions)" iterations) (fun () -> runOptimizedSyncHfsm iterations |> ignore) ]

printfn "HFSM benchmark baseline"
printfn "samples: %d" samples
printfn "iterations per sample: %d" iterations
printfn ""

results |> List.iter printResult