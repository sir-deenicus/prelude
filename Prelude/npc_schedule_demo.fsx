#nowarn "1125"

#I @"..\..\DictionarySlim\bin\Release\netstandard2.1"
#r "DictionarySlim.dll"
#r @"bin\Debug\net5\Prelude.dll"

open Prelude.Control

type NpcActivity =
    | Sleep
    | Work
    | Relax

type NpcState =
    | ChooseNextActivity
    | Perform of NpcActivity
    | Finished

type NpcStateKey =
    | ChooseKey
    | PerformKey
    | SleepKey
    | WorkKey
    | RelaxKey
    | FinishedKey

type DailySchedule =
    { SleepStart: int
      SleepEnd: int
      WorkStart: int
      WorkEnd: int }

type NpcMemory =
    { Name: string
      Hour: int
      Schedule: DailySchedule
      CurrentActivity: NpcActivity option }

let hourInWindow startHour endHour hour =
    if startHour <= endHour then
        hour >= startHour && hour < endHour
    else
        hour >= startHour || hour < endHour

let activityForHour schedule hour =
    if hourInWindow schedule.SleepStart schedule.SleepEnd hour then
        Sleep
    elif hourInWindow schedule.WorkStart schedule.WorkEnd hour then
        Work
    else
        Relax

let npcStateKey = function
    | ChooseNextActivity -> ChooseKey
    | Perform Sleep -> SleepKey
    | Perform Work -> WorkKey
    | Perform Relax -> RelaxKey
    | Finished -> FinishedKey

// One compiled behavior definition can be shared across many NPCs.
let villagerBehavior : OptimizedStateMachine<NpcState, NpcStateKey, NpcMemory> =
    optimizedStateMachine {
        exit_state Finished
        state_key npcStateKey

        exact ChooseNextActivity (fun mem ->
            let nextActivity = activityForHour mem.Schedule mem.Hour
            { NextState = Perform nextActivity; Mem = mem })

        children PerformKey [ SleepKey; WorkKey; RelaxKey ]

        hierarchy PerformKey (fun state mem ->
            match state with
            | Perform activity ->
                { NextState = Finished
                  Mem = { mem with CurrentActivity = Some activity } }
            | _ ->
                invalidOp (sprintf "Expected Perform state, got %A" state))
    }

let describeNpc hour name schedule =
    let initialMem =
        { Name = name
          Hour = hour
          Schedule = schedule
          CurrentActivity = None }

    let instance = villagerBehavior.CreateInstance()
    let status = instance.Run({ NextState = ChooseNextActivity; Mem = initialMem })

    match status.StopReason with
    | ReachedExit transition ->
        match transition.Mem.CurrentActivity with
        | Some activity ->
            printfn "%s at %02d:00 -> %A" transition.Mem.Name transition.Mem.Hour activity
        | None ->
            invalidOp "Expected the NPC activity to be assigned before exit."
    | stopReason ->
        invalidOp (sprintf "Expected ReachedExit, got %A" stopReason)

let bakerSchedule =
    { SleepStart = 21
      SleepEnd = 5
      WorkStart = 6
      WorkEnd = 14 }

let guardSchedule =
    { SleepStart = 3
      SleepEnd = 11
      WorkStart = 18
      WorkEnd = 2 }

printfn "Shared machine definition, different per-NPC schedules:"
printfn ""

for hour in [ 7; 20 ] do
    printfn "Hour %02d:00" hour
    describeNpc hour "Baker" bakerSchedule
    describeNpc hour "Night guard" guardSchedule
    printfn ""