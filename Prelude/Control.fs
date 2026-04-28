module Prelude.Control

open System.Collections.Concurrent
open System.Collections.Generic

type Actor<'a> = MailboxProcessor<'a>

type TransitionMsg<'a, 'b> = { NextState: 'a; Mem: 'b }

[<Struct>]
type StepOutcome<'state, 'mem, 'wait> =
    | Continue of nextTransition: TransitionMsg<'state, 'mem>
    | Yield of nextTransition: TransitionMsg<'state, 'mem>
    | Wait of waitReason: 'wait

type ExecutionPolicy<'state, 'mem> =
    | RunToStable
    | MaxTransitions of int
    | SingleStep
    | Until of (('state * 'mem) option -> bool)

type ExecutionStopReason<'state, 'mem, 'wait> =
    | ReachedExit of TransitionMsg<'state, 'mem>
    | Yielded
    | Waiting of 'wait
    | TransitionBudgetReached
    | PredicateSatisfied
    | NoCurrentConfig

type MachineStatus<'state, 'mem, 'wait> =
    { CurrentConfig: ('state * 'mem) option
      PendingTransition: TransitionMsg<'state, 'mem> option
      TransitionsProcessed: int
      StopReason: ExecutionStopReason<'state, 'mem, 'wait> }

type ExecutionRequest<'state, 'mem> =
    { Transition: TransitionMsg<'state, 'mem>
      Policy: ExecutionPolicy<'state, 'mem> }

type private ExecutionCommand<'state, 'mem, 'wait> =
    | FireAndForget of ExecutionRequest<'state, 'mem>
    | ExecuteAndReply of ExecutionRequest<'state, 'mem> * AsyncReplyChannel<MachineStatus<'state, 'mem, 'wait>>

type SteppableStateMachineExec<'state, 'mem, 'wait when 'state: equality>(ExitState: 'state) =
    let transitionFns = Dictionary<'state, 'mem -> StepOutcome<'state, 'mem, 'wait>>()
    let resolvedTransitionFns = Dictionary<'state, 'mem -> StepOutcome<'state, 'mem, 'wait>>()
    let transitionMatchers = ResizeArray<int * ('state -> voption<'mem -> StepOutcome<'state, 'mem, 'wait>>)>()
    let mutable nextTransitionMatcherId = 0

    let mutable currentState : 'state option = None
    let mutable currentMem : 'mem option = None
    let mutable pendingTransition : TransitionMsg<'state, 'mem> option = None

    let msgEvent = Event<_>()

    let invalidateResolvedTransitions () =
        resolvedTransitionFns.Clear()

    let currentConfig () =
        match currentState, currentMem with
        | Some state, Some mem -> Some(state, mem)
        | _ -> None

    let createStatus stopReason transitionsProcessed =
        { CurrentConfig = currentConfig ()
          PendingTransition = pendingTransition
          TransitionsProcessed = transitionsProcessed
          StopReason = stopReason }

    let tryGetPolicyStopReason policy transitionsProcessed candidateConfig =
        match policy with
        | RunToStable -> None
        | MaxTransitions maxTransitions when maxTransitions <= 0 -> Some TransitionBudgetReached
        | MaxTransitions maxTransitions when transitionsProcessed >= maxTransitions -> Some TransitionBudgetReached
        | SingleStep when transitionsProcessed >= 1 -> Some TransitionBudgetReached
        | Until predicate when predicate candidateConfig -> Some PredicateSatisfied
        | _ -> None

    let tryResolveOutcome state mem =
        match transitionMatchers.Count with
        | 0 ->
            let found, transition = transitionFns.TryGetValue state
            if found then ValueSome(transition mem) else ValueNone
        | _ ->
            let cachedFound, cachedTransition = resolvedTransitionFns.TryGetValue state
            if cachedFound then ValueSome(cachedTransition mem)
            else
                let found, transition = transitionFns.TryGetValue state
                if found then ValueSome(transition mem)
                else
                    match transitionMatchers.Count with
                    | 1 ->
                        let _, transition = transitionMatchers.[0]
                        match transition state with
                        | ValueSome resolvedTransition ->
                            resolvedTransitionFns[state] <- resolvedTransition
                            ValueSome(resolvedTransition mem)
                        | ValueNone -> ValueNone
                    | _ ->
                        let mutable resolved = ValueNone
                        let mutable index = 0

                        while index < transitionMatchers.Count && ValueOption.isNone resolved do
                            let _, transition = transitionMatchers.[index]
                            resolved <- transition state
                            index <- index + 1

                        match resolved with
                        | ValueSome resolvedTransition ->
                            resolvedTransitionFns[state] <- resolvedTransition
                            ValueSome(resolvedTransition mem)
                        | ValueNone -> ValueNone

    let runToStable initialTransition =
        pendingTransition <- None

        let mutable transition = initialTransition
        let mutable transitionsProcessed = 0
        let mutable keepRunning = true
        let mutable status = createStatus NoCurrentConfig 0

        while keepRunning do
            if transition.NextState = ExitState then
                pendingTransition <- None
                status <- createStatus (ReachedExit transition) transitionsProcessed
                keepRunning <- false
            else
                currentState <- Some transition.NextState
                currentMem <- Some transition.Mem
                msgEvent.Trigger transition.Mem

                match tryResolveOutcome transition.NextState transition.Mem with
                | ValueSome(Continue nextTransition) ->
                    transitionsProcessed <- transitionsProcessed + 1
                    transition <- nextTransition
                | ValueSome(Yield nextTransition) ->
                    transitionsProcessed <- transitionsProcessed + 1

                    if nextTransition.NextState = ExitState then
                        pendingTransition <- None
                        status <- createStatus (ReachedExit nextTransition) transitionsProcessed
                    else
                        pendingTransition <- Some nextTransition
                        status <- createStatus Yielded transitionsProcessed

                    keepRunning <- false
                | ValueSome(Wait waitReason) ->
                    pendingTransition <- None
                    status <- createStatus (Waiting waitReason) transitionsProcessed
                    keepRunning <- false
                | ValueNone -> invalidOp (sprintf "No transition registered for state %A" transition.NextState)

        status

    let runFrom initialTransition policy =
        match policy with
        | RunToStable -> runToStable initialTransition
        | _ ->
            pendingTransition <- None

            match tryGetPolicyStopReason policy 0 (Some(initialTransition.NextState, initialTransition.Mem)) with
            | Some stopReason when initialTransition.NextState <> ExitState ->
                pendingTransition <- Some initialTransition
                createStatus stopReason 0
            | _ ->
                let mutable transition = initialTransition
                let mutable transitionsProcessed = 0
                let mutable keepRunning = true
                let mutable status = createStatus NoCurrentConfig 0

                while keepRunning do
                    if transition.NextState = ExitState then
                        pendingTransition <- None
                        status <- createStatus (ReachedExit transition) transitionsProcessed
                        keepRunning <- false
                    else
                        currentState <- Some transition.NextState
                        currentMem <- Some transition.Mem
                        msgEvent.Trigger transition.Mem

                        match tryResolveOutcome transition.NextState transition.Mem with
                        | ValueSome(Wait waitReason) ->
                            pendingTransition <- None
                            status <- createStatus (Waiting waitReason) transitionsProcessed
                            keepRunning <- false
                        | ValueSome(Continue nextTransition) ->
                            transitionsProcessed <- transitionsProcessed + 1

                            if nextTransition.NextState = ExitState then
                                pendingTransition <- None
                                status <- createStatus (ReachedExit nextTransition) transitionsProcessed
                                keepRunning <- false
                            else
                                match tryGetPolicyStopReason policy transitionsProcessed (Some(nextTransition.NextState, nextTransition.Mem)) with
                                | Some stopReason ->
                                    pendingTransition <- Some nextTransition
                                    status <- createStatus stopReason transitionsProcessed
                                    keepRunning <- false
                                | None ->
                                    transition <- nextTransition
                        | ValueSome(Yield nextTransition) ->
                            transitionsProcessed <- transitionsProcessed + 1

                            if nextTransition.NextState = ExitState then
                                pendingTransition <- None
                                status <- createStatus (ReachedExit nextTransition) transitionsProcessed
                            else
                                pendingTransition <- Some nextTransition
                                status <- createStatus Yielded transitionsProcessed

                            keepRunning <- false
                        | ValueNone -> invalidOp (sprintf "No transition registered for state %A" transition.NextState)

                status

    let nextTransitionFromCurrent () =
        match pendingTransition, currentConfig () with
        | Some transition, _ -> Some transition
        | None, Some(state, mem) -> Some { NextState = state; Mem = mem }
        | None, None -> None

    let main (exec: Actor<ExecutionCommand<'state, 'mem, 'wait>>) =
        let rec loop () = async {
                let! command = exec.Receive()

                match command with
                | FireAndForget request ->
                    runFrom request.Transition request.Policy |> ignore
                | ExecuteAndReply(request, reply) ->
                    runFrom request.Transition request.Policy |> reply.Reply

                return! (loop ())
            }

        loop ()

    let statemachine = Actor.Start(main)

    let registerOutcome state f =
        if not (transitionFns.ContainsKey state) then
            transitionFns.Add(state, f)
            resolvedTransitionFns.Remove(state) |> ignore

    let registerWhenOutcome matches f =
        let matcherId = nextTransitionMatcherId
        nextTransitionMatcherId <- nextTransitionMatcherId + 1
        transitionMatchers.Add(matcherId, fun state -> if matches state then ValueSome f else ValueNone)
        invalidateResolvedTransitions ()
        matcherId

    let registerCaseOutcome tryMatch f =
        let matcherId = nextTransitionMatcherId
        nextTransitionMatcherId <- nextTransitionMatcherId + 1
        transitionMatchers.Add(matcherId, fun state ->
            match tryMatch state with
            | Some caseValue -> ValueSome(fun mem -> f caseValue mem)
            | None -> ValueNone)
        invalidateResolvedTransitions ()
        matcherId

    member __.Post(msg) =
        statemachine.Post(FireAndForget { Transition = msg; Policy = RunToStable })

    member __.PostWithPolicy(msg, policy) =
        statemachine.Post(FireAndForget { Transition = msg; Policy = policy })

    member __.RunOnActor(msg, ?policy) =
        let policy = defaultArg policy RunToStable
        statemachine.PostAndReply(fun reply -> ExecuteAndReply({ Transition = msg; Policy = policy }, reply))

    member __.Run(msg, ?policy) =
        runFrom msg (defaultArg policy RunToStable)

    member __.RunSingleStep(msg) =
        runFrom msg SingleStep

    member __.RunFor(msg, maxTransitions) =
        runFrom msg (MaxTransitions maxTransitions)

    member __.RunUntil(msg, predicate) =
        runFrom msg (Until predicate)

    member __.StepCurrent(?policy) =
        match nextTransitionFromCurrent () with
        | Some transition -> runFrom transition (defaultArg policy RunToStable)
        | None -> createStatus NoCurrentConfig 0

    member __.StepSingle() =
        match nextTransitionFromCurrent () with
        | Some transition -> runFrom transition SingleStep
        | None -> createStatus NoCurrentConfig 0

    member __.StepFor(maxTransitions) =
        match nextTransitionFromCurrent () with
        | Some transition -> runFrom transition (MaxTransitions maxTransitions)
        | None -> createStatus NoCurrentConfig 0

    member __.StepUntil(predicate) =
        match nextTransitionFromCurrent () with
        | Some transition -> runFrom transition (Until predicate)
        | None -> createStatus NoCurrentConfig 0

    member __.MemStream = msgEvent.Publish

    member __.CurrentState = currentState
    member __.CurrentMem = currentMem
    member __.CurrentConfig = currentConfig ()
    member __.PendingTransition = pendingTransition

    member __.UnRegister(state) =  
        if transitionFns.ContainsKey state then
            transitionFns.Remove(state) |> ignore
            resolvedTransitionFns.Remove(state) |> ignore

    member __.RegisterOutcome(state, f) =
        registerOutcome state f

    member __.Register(state, f) =
        registerOutcome state (fun mem -> Continue(f mem))

    member __.RegisterWhenOutcome(matches, f) =
        registerWhenOutcome matches f

    member __.RegisterWhen(matches, f) =
        registerWhenOutcome matches (fun mem -> Continue(f mem))

    member __.RegisterCaseOutcome(tryMatch, f) =
        registerCaseOutcome tryMatch f

    member __.RegisterCase(tryMatch, f) =
        registerCaseOutcome tryMatch (fun caseValue mem -> Continue(f caseValue mem))

    member __.UnRegisterWhen(matcherId) =
        transitionMatchers.RemoveAll(fun (registeredId, _) -> registeredId = matcherId)
        |> ignore
        invalidateResolvedTransitions ()

type StateMachineExec<'state, 'mem when 'state: equality>(ExitState: 'state) =
    let stateMachine = SteppableStateMachineExec<'state, 'mem, unit>(ExitState)

    member __.Post(msg) = stateMachine.Post msg

    member __.PostWithPolicy(msg, policy) =
        stateMachine.PostWithPolicy(msg, policy)

    member __.Run(msg, ?policy) =
        stateMachine.Run(msg, ?policy = policy)

    member __.RunOnActor(msg, ?policy) =
        stateMachine.RunOnActor(msg, ?policy = policy)

    member __.RunSingleStep(msg) =
        stateMachine.RunSingleStep(msg)

    member __.RunFor(msg, maxTransitions) =
        stateMachine.RunFor(msg, maxTransitions)

    member __.RunUntil(msg, predicate) =
        stateMachine.RunUntil(msg, predicate)

    member __.StepCurrent(?policy) =
        stateMachine.StepCurrent(?policy = policy)

    member __.StepSingle() =
        stateMachine.StepSingle()

    member __.StepFor(maxTransitions) =
        stateMachine.StepFor(maxTransitions)

    member __.StepUntil(predicate) =
        stateMachine.StepUntil(predicate)

    member __.MemStream = stateMachine.MemStream

    member __.CurrentState = stateMachine.CurrentState
    member __.CurrentMem = stateMachine.CurrentMem
    member __.CurrentConfig = stateMachine.CurrentConfig
    member __.PendingTransition = stateMachine.PendingTransition

    member __.UnRegister(state) =
        stateMachine.UnRegister(state)

    member __.Register(state, f) =
        stateMachine.Register(state, f)

    member __.RegisterOutcome(state, f) =
        stateMachine.RegisterOutcome(state, f)

    member __.RegisterWhen(matches, f) =
        stateMachine.RegisterWhen(matches, f)

    member __.RegisterWhenOutcome(matches, f) =
        stateMachine.RegisterWhenOutcome(matches, f)

    member __.RegisterCase(tryMatch, f) =
        stateMachine.RegisterCase(tryMatch, f)

    member __.RegisterCaseOutcome(tryMatch, f) =
        stateMachine.RegisterCaseOutcome(tryMatch, f)

    member __.UnRegisterWhen(matcherId) =
        stateMachine.UnRegisterWhen(matcherId)

type OptimizedStateMachineExec<'state, 'hierarchyKey, 'mem when 'state: equality and 'hierarchyKey: equality>(ExitState: 'state, GetHierarchyKey: 'state -> 'hierarchyKey) =
    let transitionFns = Dictionary<'state, 'mem -> TransitionMsg<'state, 'mem>>()
    let hierarchyTransitionFns = Dictionary<'hierarchyKey, 'state -> 'mem -> TransitionMsg<'state, 'mem>>()
    let resolvedHierarchyFns = Dictionary<'state, 'state -> 'mem -> TransitionMsg<'state, 'mem>>()
    let parentKeys = Dictionary<'hierarchyKey, 'hierarchyKey>()

    let mutable currentState : 'state option = None
    let mutable currentMem : 'mem option = None
    let mutable pendingTransition : TransitionMsg<'state, 'mem> option = None

    let msgEvent = Event<_>()

    let invalidateResolvedTransitions () =
        resolvedHierarchyFns.Clear()

    let currentConfig () =
        match currentState, currentMem with
        | Some state, Some mem -> Some(state, mem)
        | _ -> None

    let createStatus stopReason transitionsProcessed =
        { CurrentConfig = currentConfig ()
          PendingTransition = pendingTransition
          TransitionsProcessed = transitionsProcessed
          StopReason = stopReason }

    let tryGetPolicyStopReason policy transitionsProcessed candidateConfig =
        match policy with
        | RunToStable -> None
        | MaxTransitions maxTransitions when maxTransitions <= 0 -> Some TransitionBudgetReached
        | MaxTransitions maxTransitions when transitionsProcessed >= maxTransitions -> Some TransitionBudgetReached
        | SingleStep when transitionsProcessed >= 1 -> Some TransitionBudgetReached
        | Until predicate when predicate candidateConfig -> Some PredicateSatisfied
        | _ -> None

    let tryResolveTransition state mem =
        match transitionFns.TryGetValue state with
        | true, transition -> Some(transition mem)
        | _ ->
            match resolvedHierarchyFns.TryGetValue state with
            | true, transition -> Some(transition state mem)
            | _ ->
                let mutable currentKey = GetHierarchyKey state
                let mutable keepSearching = true
                let mutable found = false
                let mutable resolvedTransition = Unchecked.defaultof<'state -> 'mem -> TransitionMsg<'state, 'mem>>

                while keepSearching && not found do
                    match hierarchyTransitionFns.TryGetValue currentKey with
                    | true, transition ->
                        resolvedTransition <- transition
                        found <- true
                    | _ ->
                        match parentKeys.TryGetValue currentKey with
                        | true, parentKey -> currentKey <- parentKey
                        | _ -> keepSearching <- false

                if found then
                    resolvedHierarchyFns[state] <- resolvedTransition
                    Some(resolvedTransition state mem)
                else
                    None

    let runToStable initialTransition =
        pendingTransition <- None

        let mutable transition = initialTransition
        let mutable transitionsProcessed = 0
        let mutable keepRunning = true
        let mutable status = createStatus NoCurrentConfig 0

        while keepRunning do
            if transition.NextState = ExitState then
                pendingTransition <- None
                status <- createStatus (ReachedExit transition) transitionsProcessed
                keepRunning <- false
            else
                currentState <- Some transition.NextState
                currentMem <- Some transition.Mem
                msgEvent.Trigger transition.Mem

                match tryResolveTransition transition.NextState transition.Mem with
                | Some nextTransition ->
                    transitionsProcessed <- transitionsProcessed + 1
                    transition <- nextTransition
                | None -> invalidOp (sprintf "No transition registered for state %A" transition.NextState)

        status

    let runFrom initialTransition policy =
        match policy with
        | RunToStable -> runToStable initialTransition
        | _ ->
            pendingTransition <- None

            match tryGetPolicyStopReason policy 0 (Some(initialTransition.NextState, initialTransition.Mem)) with
            | Some stopReason when initialTransition.NextState <> ExitState ->
                pendingTransition <- Some initialTransition
                createStatus stopReason 0
            | _ ->
                let mutable transition = initialTransition
                let mutable transitionsProcessed = 0
                let mutable keepRunning = true
                let mutable status = createStatus NoCurrentConfig 0

                while keepRunning do
                    if transition.NextState = ExitState then
                        pendingTransition <- None
                        status <- createStatus (ReachedExit transition) transitionsProcessed
                        keepRunning <- false
                    else
                        currentState <- Some transition.NextState
                        currentMem <- Some transition.Mem
                        msgEvent.Trigger transition.Mem

                        match tryResolveTransition transition.NextState transition.Mem with
                        | Some nextTransition ->
                            transitionsProcessed <- transitionsProcessed + 1

                            if nextTransition.NextState = ExitState then
                                pendingTransition <- None
                                status <- createStatus (ReachedExit nextTransition) transitionsProcessed
                                keepRunning <- false
                            else
                                match tryGetPolicyStopReason policy transitionsProcessed (Some(nextTransition.NextState, nextTransition.Mem)) with
                                | Some stopReason ->
                                    pendingTransition <- Some nextTransition
                                    status <- createStatus stopReason transitionsProcessed
                                    keepRunning <- false
                                | None ->
                                    transition <- nextTransition
                        | None -> invalidOp (sprintf "No transition registered for state %A" transition.NextState)

                status

    let nextTransitionFromCurrent () =
        match pendingTransition, currentConfig () with
        | Some transition, _ -> Some transition
        | None, Some(state, mem) -> Some { NextState = state; Mem = mem }
        | None, None -> None

    let main (exec: Actor<ExecutionCommand<'state, 'mem, unit>>) =
        let rec loop () = async {
                let! command = exec.Receive()

                match command with
                | FireAndForget request ->
                    runFrom request.Transition request.Policy |> ignore
                | ExecuteAndReply(request, reply) ->
                    runFrom request.Transition request.Policy |> reply.Reply

                return! (loop ())
            }

        loop ()

    let statemachine = lazy (Actor.Start(main))

    let ensureActor () =
        statemachine.Force() |> ignore

    member __.Post(msg) =
        ensureActor ()
        statemachine.Value.Post(FireAndForget { Transition = msg; Policy = RunToStable })

    member __.PostWithPolicy(msg, policy) =
        ensureActor ()
        statemachine.Value.Post(FireAndForget { Transition = msg; Policy = policy })

    member __.StartActor() =
        ensureActor ()

    member __.RunOnActor(msg, ?policy) =
        let policy = defaultArg policy RunToStable
        ensureActor ()
        statemachine.Value.PostAndReply(fun reply -> ExecuteAndReply({ Transition = msg; Policy = policy }, reply))

    member __.Run(msg, ?policy) =
        runFrom msg (defaultArg policy RunToStable)

    member __.RunSingleStep(msg) =
        runFrom msg SingleStep

    member __.RunFor(msg, maxTransitions) =
        runFrom msg (MaxTransitions maxTransitions)

    member __.RunUntil(msg, predicate) =
        runFrom msg (Until predicate)

    member __.StepCurrent(?policy) =
        match nextTransitionFromCurrent () with
        | Some transition -> runFrom transition (defaultArg policy RunToStable)
        | None -> createStatus NoCurrentConfig 0

    member __.StepSingle() =
        match nextTransitionFromCurrent () with
        | Some transition -> runFrom transition SingleStep
        | None -> createStatus NoCurrentConfig 0

    member __.StepFor(maxTransitions) =
        match nextTransitionFromCurrent () with
        | Some transition -> runFrom transition (MaxTransitions maxTransitions)
        | None -> createStatus NoCurrentConfig 0

    member __.StepUntil(predicate) =
        match nextTransitionFromCurrent () with
        | Some transition -> runFrom transition (Until predicate)
        | None -> createStatus NoCurrentConfig 0

    member __.MemStream = msgEvent.Publish

    member __.CurrentState = currentState
    member __.CurrentMem = currentMem
    member __.CurrentConfig = currentConfig ()
    member __.PendingTransition = pendingTransition

    member __.Register(state, f) =
        transitionFns[state] <- f
        resolvedHierarchyFns.Remove(state) |> ignore

    member __.RegisterHierarchy(key, f) =
        hierarchyTransitionFns[key] <- f
        invalidateResolvedTransitions ()

    member __.RegisterParent(childKey, parentKey) =
        parentKeys[childKey] <- parentKey
        invalidateResolvedTransitions ()

    member __.UnRegister(state) =
        if transitionFns.ContainsKey state then
            transitionFns.Remove(state) |> ignore
            resolvedHierarchyFns.Remove(state) |> ignore

    member __.UnRegisterHierarchy(key) =
        if hierarchyTransitionFns.ContainsKey key then
            hierarchyTransitionFns.Remove(key) |> ignore
            invalidateResolvedTransitions ()

    member __.UnRegisterParent(childKey) =
        if parentKeys.ContainsKey childKey then
            parentKeys.Remove(childKey) |> ignore
            invalidateResolvedTransitions ()

type internal OptimizedCompiledStateMachine<'state, 'hierarchyKey, 'mem when 'state: equality and 'hierarchyKey: equality>(
    ExitState: 'state,
    GetHierarchyKey: 'state -> 'hierarchyKey,
    ExactTransitions: seq<'state * ('mem -> TransitionMsg<'state, 'mem>)>,
    HierarchyTransitions: seq<'hierarchyKey * ('state -> 'mem -> TransitionMsg<'state, 'mem>)>,
    ParentKeys: seq<'hierarchyKey * 'hierarchyKey>) =

    let transitionFns = Dictionary<'state, 'mem -> TransitionMsg<'state, 'mem>>()
    let hierarchyTransitionFns = Dictionary<'hierarchyKey, 'state -> 'mem -> TransitionMsg<'state, 'mem>>()
    let parentKeys = Dictionary<'hierarchyKey, 'hierarchyKey>()
    let resolvedHierarchyFns = ConcurrentDictionary<'state, 'state -> 'mem -> TransitionMsg<'state, 'mem>>()

    do
        for state, transition in ExactTransitions do
            transitionFns[state] <- transition

        for key, transition in HierarchyTransitions do
            hierarchyTransitionFns[key] <- transition

        for childKey, parentKey in ParentKeys do
            parentKeys[childKey] <- parentKey

    member __.ExitState = ExitState

    member __.TryResolveTransition(state, mem) =
        match transitionFns.TryGetValue state with
        | true, transition -> Some(transition mem)
        | _ ->
            match resolvedHierarchyFns.TryGetValue state with
            | true, transition -> Some(transition state mem)
            | _ ->
                let mutable currentKey = GetHierarchyKey state
                let mutable keepSearching = true
                let mutable found = false
                let mutable resolvedTransition = Unchecked.defaultof<'state -> 'mem -> TransitionMsg<'state, 'mem>>

                while keepSearching && not found do
                    match hierarchyTransitionFns.TryGetValue currentKey with
                    | true, transition ->
                        resolvedTransition <- transition
                        found <- true
                    | _ ->
                        match parentKeys.TryGetValue currentKey with
                        | true, parentKey -> currentKey <- parentKey
                        | _ -> keepSearching <- false

                if found then
                    resolvedHierarchyFns.TryAdd(state, resolvedTransition) |> ignore
                    Some(resolvedTransition state mem)
                else
                    None

type OptimizedStateMachineInstance<'state, 'hierarchyKey, 'mem when 'state: equality and 'hierarchyKey: equality> internal (CompiledDefinition: OptimizedCompiledStateMachine<'state, 'hierarchyKey, 'mem>) =
    let mutable currentState : 'state option = None
    let mutable currentMem : 'mem option = None
    let mutable pendingTransition : TransitionMsg<'state, 'mem> option = None

    let msgEvent = Event<_>()

    let currentConfig () =
        match currentState, currentMem with
        | Some state, Some mem -> Some(state, mem)
        | _ -> None

    let createStatus stopReason transitionsProcessed =
        { CurrentConfig = currentConfig ()
          PendingTransition = pendingTransition
          TransitionsProcessed = transitionsProcessed
          StopReason = stopReason }

    let tryGetPolicyStopReason policy transitionsProcessed candidateConfig =
        match policy with
        | RunToStable -> None
        | MaxTransitions maxTransitions when maxTransitions <= 0 -> Some TransitionBudgetReached
        | MaxTransitions maxTransitions when transitionsProcessed >= maxTransitions -> Some TransitionBudgetReached
        | SingleStep when transitionsProcessed >= 1 -> Some TransitionBudgetReached
        | Until predicate when predicate candidateConfig -> Some PredicateSatisfied
        | _ -> None

    let runToStable initialTransition =
        pendingTransition <- None

        let mutable transition = initialTransition
        let mutable transitionsProcessed = 0
        let mutable keepRunning = true
        let mutable status = createStatus NoCurrentConfig 0

        while keepRunning do
            if transition.NextState = CompiledDefinition.ExitState then
                pendingTransition <- None
                status <- createStatus (ReachedExit transition) transitionsProcessed
                keepRunning <- false
            else
                currentState <- Some transition.NextState
                currentMem <- Some transition.Mem
                msgEvent.Trigger transition.Mem

                match CompiledDefinition.TryResolveTransition(transition.NextState, transition.Mem) with
                | Some nextTransition ->
                    transitionsProcessed <- transitionsProcessed + 1
                    transition <- nextTransition
                | None -> invalidOp (sprintf "No transition registered for state %A" transition.NextState)

        status

    let runFrom initialTransition policy =
        match policy with
        | RunToStable -> runToStable initialTransition
        | _ ->
            pendingTransition <- None

            match tryGetPolicyStopReason policy 0 (Some(initialTransition.NextState, initialTransition.Mem)) with
            | Some stopReason when initialTransition.NextState <> CompiledDefinition.ExitState ->
                pendingTransition <- Some initialTransition
                createStatus stopReason 0
            | _ ->
                let mutable transition = initialTransition
                let mutable transitionsProcessed = 0
                let mutable keepRunning = true
                let mutable status = createStatus NoCurrentConfig 0

                while keepRunning do
                    if transition.NextState = CompiledDefinition.ExitState then
                        pendingTransition <- None
                        status <- createStatus (ReachedExit transition) transitionsProcessed
                        keepRunning <- false
                    else
                        currentState <- Some transition.NextState
                        currentMem <- Some transition.Mem
                        msgEvent.Trigger transition.Mem

                        match CompiledDefinition.TryResolveTransition(transition.NextState, transition.Mem) with
                        | Some nextTransition ->
                            transitionsProcessed <- transitionsProcessed + 1

                            if nextTransition.NextState = CompiledDefinition.ExitState then
                                pendingTransition <- None
                                status <- createStatus (ReachedExit nextTransition) transitionsProcessed
                                keepRunning <- false
                            else
                                match tryGetPolicyStopReason policy transitionsProcessed (Some(nextTransition.NextState, nextTransition.Mem)) with
                                | Some stopReason ->
                                    pendingTransition <- Some nextTransition
                                    status <- createStatus stopReason transitionsProcessed
                                    keepRunning <- false
                                | None ->
                                    transition <- nextTransition
                        | None -> invalidOp (sprintf "No transition registered for state %A" transition.NextState)

                status

    let nextTransitionFromCurrent () =
        match pendingTransition, currentConfig () with
        | Some transition, _ -> Some transition
        | None, Some(state, mem) -> Some { NextState = state; Mem = mem }
        | None, None -> None

    let main (exec: Actor<ExecutionCommand<'state, 'mem, unit>>) =
        let rec loop () = async {
                let! command = exec.Receive()

                match command with
                | FireAndForget request ->
                    runFrom request.Transition request.Policy |> ignore
                | ExecuteAndReply(request, reply) ->
                    runFrom request.Transition request.Policy |> reply.Reply

                return! (loop ())
            }

        loop ()

    let statemachine = lazy (Actor.Start(main))

    let ensureActor () =
        statemachine.Force() |> ignore

    member __.StartActor() =
        ensureActor ()

    member __.Post(msg) =
        ensureActor ()
        statemachine.Value.Post(FireAndForget { Transition = msg; Policy = RunToStable })

    member __.PostWithPolicy(msg, policy) =
        ensureActor ()
        statemachine.Value.Post(FireAndForget { Transition = msg; Policy = policy })

    member __.RunOnActor(msg, ?policy) =
        let policy = defaultArg policy RunToStable
        ensureActor ()
        statemachine.Value.PostAndReply(fun reply -> ExecuteAndReply({ Transition = msg; Policy = policy }, reply))

    member __.Run(msg, ?policy) =
        runFrom msg (defaultArg policy RunToStable)

    member __.RunSingleStep(msg) =
        runFrom msg SingleStep

    member __.RunFor(msg, maxTransitions) =
        runFrom msg (MaxTransitions maxTransitions)

    member __.RunUntil(msg, predicate) =
        runFrom msg (Until predicate)

    member __.StepCurrent(?policy) =
        match nextTransitionFromCurrent () with
        | Some transition -> runFrom transition (defaultArg policy RunToStable)
        | None -> createStatus NoCurrentConfig 0

    member __.StepSingle() =
        match nextTransitionFromCurrent () with
        | Some transition -> runFrom transition SingleStep
        | None -> createStatus NoCurrentConfig 0

    member __.StepFor(maxTransitions) =
        match nextTransitionFromCurrent () with
        | Some transition -> runFrom transition (MaxTransitions maxTransitions)
        | None -> createStatus NoCurrentConfig 0

    member __.StepUntil(predicate) =
        match nextTransitionFromCurrent () with
        | Some transition -> runFrom transition (Until predicate)
        | None -> createStatus NoCurrentConfig 0

    member __.MemStream = msgEvent.Publish

    member __.CurrentState = currentState
    member __.CurrentMem = currentMem
    member __.CurrentConfig = currentConfig ()
    member __.PendingTransition = pendingTransition

type OptimizedStateMachine<'state, 'hierarchyKey, 'mem when 'state: equality and 'hierarchyKey: equality>(
    ExitState: 'state,
    GetHierarchyKey: 'state -> 'hierarchyKey,
    ExactTransitions: seq<'state * ('mem -> TransitionMsg<'state, 'mem>)>,
    HierarchyTransitions: seq<'hierarchyKey * ('state -> 'mem -> TransitionMsg<'state, 'mem>)>,
    ParentKeys: seq<'hierarchyKey * 'hierarchyKey>) =

    let compiledDefinition =
        OptimizedCompiledStateMachine<'state, 'hierarchyKey, 'mem>(
            ExitState,
            GetHierarchyKey,
            ExactTransitions,
            HierarchyTransitions,
            ParentKeys)

    member __.ExitState = ExitState
    member __.GetHierarchyKey = GetHierarchyKey

    member __.CreateInstance(?startActor) : OptimizedStateMachineInstance<'state, 'hierarchyKey, 'mem> =
        let instance : OptimizedStateMachineInstance<'state, 'hierarchyKey, 'mem> =
            OptimizedStateMachineInstance<'state, 'hierarchyKey, 'mem>(compiledDefinition)

        if defaultArg startActor false then
            instance.StartActor()

        instance

type OptimizedStateMachineSpec<'state, 'hierarchyKey, 'mem when 'state: equality and 'hierarchyKey: equality> =
    { ExitState: 'state option
      GetHierarchyKey: ('state -> 'hierarchyKey) option
      ExactTransitions: ResizeArray<'state * ('mem -> TransitionMsg<'state, 'mem>)>
      HierarchyTransitions: ResizeArray<'hierarchyKey * ('state -> 'mem -> TransitionMsg<'state, 'mem>)>
      ParentKeys: ResizeArray<'hierarchyKey * 'hierarchyKey> }

type OptimizedStateMachineBuilder() =
    member private __.EmptySpec<'state, 'hierarchyKey, 'mem when 'state: equality and 'hierarchyKey: equality>() : OptimizedStateMachineSpec<'state, 'hierarchyKey, 'mem> =
        { ExitState = None
          GetHierarchyKey = None
          ExactTransitions = ResizeArray()
          HierarchyTransitions = ResizeArray()
          ParentKeys = ResizeArray() }

    member this.Yield(_) =
        this.EmptySpec()

    member this.Zero() =
        this.EmptySpec()

    member __.Delay(f) =
        f()

    [<CustomOperation("exit_state")>]
    member __.ExitState(spec, exitState) =
        { spec with ExitState = Some exitState }

    [<CustomOperation("state_key")>]
    member __.StateKey(spec, getHierarchyKey) =
        { spec with GetHierarchyKey = Some getHierarchyKey }

    [<CustomOperation("exact")>]
    member __.Exact(spec, state, f) =
        spec.ExactTransitions.Add(state, f)
        spec

    [<CustomOperation("exacts")>]
    member __.Exacts(spec, transitions: seq<'state * ('mem -> TransitionMsg<'state, 'mem>)>) =
        for state, transition in transitions do
            spec.ExactTransitions.Add(state, transition)

        spec

    [<CustomOperation("hierarchy")>]
    member __.Hierarchy(spec, key, f) =
        spec.HierarchyTransitions.Add(key, f)
        spec

    [<CustomOperation("parent")>]
    member __.Parent(spec, childKey, parentKey) =
        spec.ParentKeys.Add(childKey, parentKey)
        spec

    [<CustomOperation("children")>]
    member __.Children(spec, parentKey, childKeys: seq<'hierarchyKey>) =
        for childKey in childKeys do
            spec.ParentKeys.Add(childKey, parentKey)

        spec

    member __.Run(spec) =
        match spec.ExitState, spec.GetHierarchyKey with
        | Some exitState, Some getHierarchyKey ->
            OptimizedStateMachine<'state, 'hierarchyKey, 'mem>(
                exitState,
                getHierarchyKey,
                spec.ExactTransitions,
                spec.HierarchyTransitions,
                spec.ParentKeys)
        | None, _ -> invalidOp "optimizedStateMachine requires an exit_state declaration"
        | _, None -> invalidOp "optimizedStateMachine requires a state_key declaration"

let optimizedStateMachine = OptimizedStateMachineBuilder()
