module Prelude.Control

open System.Collections.Generic

type Actor<'a> = MailboxProcessor<'a>

type TransitionMsg<'a, 'b> = { NextState: 'a; Mem: 'b }

type StateMachineExec<'state, 'mem when 'state: equality>(ExitState: 'state) =
    let transitionFns = Dictionary<'state, 'mem -> TransitionMsg<'state, 'mem>>()
    let transitionMatchers = ResizeArray<int * ('state -> 'mem -> voption<TransitionMsg<'state, 'mem>>)>()
    let mutable nextTransitionMatcherId = 0

    let mutable currentState : 'state option = None
    let mutable currentMem : 'mem option = None

    let msgEvent = Event<_>()

    let tryResolveTransition state mem =
        let found, transition = transitionFns.TryGetValue state
        if found then ValueSome(transition mem)
        else
            let mutable resolved = ValueNone
            let mutable index = 0

            while index < transitionMatchers.Count && ValueOption.isNone resolved do
                let _, transition = transitionMatchers.[index]
                resolved <- transition state mem
                index <- index + 1

            resolved
     
    let processTransitions initialTransition =
        let mutable transition = initialTransition
        let mutable keepRunning = true

        while keepRunning do
            if transition.NextState = ExitState then
                keepRunning <- false
            else
                currentState <- Some transition.NextState
                currentMem <- Some transition.Mem
                msgEvent.Trigger transition.Mem
                transition <-
                    match tryResolveTransition transition.NextState transition.Mem with
                    | ValueSome nextTransition -> nextTransition
                    | ValueNone -> invalidOp (sprintf "No transition registered for state %A" transition.NextState)

    let main (exec: Actor<TransitionMsg<'state, 'mem>>) =
        let rec loop () = async {
                let! m = exec.Receive()
                processTransitions m
                return! (loop ())
            }

        loop ()

    let statemachine = Actor.Start(main)

    member __.Post(msg) = statemachine.Post msg

    member __.MemStream = msgEvent.Publish

    member __.CurrentState = currentState
    member __.CurrentMem = currentMem
    member __.CurrentConfig =
        match currentState, currentMem with
        | Some state, Some mem -> Some(state, mem)
        | _ -> None

    member __.UnRegister(state) =  
        if transitionFns.ContainsKey state then
            transitionFns.Remove(state) |> ignore

    member __.Register(state, f) =  
        if not (transitionFns.ContainsKey state) then
            transitionFns.Add(state, f)

    member __.RegisterWhen(matches, f) =
        let matcherId = nextTransitionMatcherId
        nextTransitionMatcherId <- nextTransitionMatcherId + 1
        transitionMatchers.Add(matcherId, fun state mem -> if matches state then ValueSome(f mem) else ValueNone)
        matcherId

    member __.RegisterCase(tryMatch, f) =
        let matcherId = nextTransitionMatcherId
        nextTransitionMatcherId <- nextTransitionMatcherId + 1
        transitionMatchers.Add(matcherId, fun state mem ->
            match tryMatch state with
            | Some caseValue -> ValueSome(f caseValue mem)
            | None -> ValueNone)
        matcherId

    member __.UnRegisterWhen(matcherId) =
        transitionMatchers.RemoveAll(fun (registeredId, _) -> registeredId = matcherId)
        |> ignore
