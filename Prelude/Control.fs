module Prelude.Control

open System.Collections.Generic

type Actor<'a> = MailboxProcessor<'a>

type TransitionMsg<'a, 'b> = { NextState: 'a; Mem: 'b }

type StateMachineExec<'state, 'mem when 'state: equality>(ExitState: 'state) =
    let transitionFns = Dictionary<'state, 'mem -> TransitionMsg<_, _>>()

    let mutable currentState = None

    let msgEvent = Event<_>()
     
    let main (exec: Actor<TransitionMsg<'state, 'mem>>) =
        let rec loop () = async {
                let! m = exec.Receive()

                if m.NextState <> ExitState then
                    currentState <- Some m.Mem
                    msgEvent.Trigger m.Mem
                    let! state' = async { return transitionFns.[m.NextState] m.Mem }
                    exec.Post state'
                    return! (loop ())
            }

        loop ()

    let statemachine = Actor.Start(main)

    member __.Post(msg) = statemachine.Post msg

    member __.MemStream = msgEvent.Publish

    member __.CurrentState = currentState

    member __.UnRegister(state) =  
        if transitionFns.ContainsKey state then
            transitionFns.Remove(state) |> ignore

    member __.Register(state, f) =  
        if not (transitionFns.ContainsKey state) then
            transitionFns.Add(state, f)
