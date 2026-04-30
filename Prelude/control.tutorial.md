# Prelude.Control Tutorial

This tutorial walks through the `Prelude.Control` state machine library from first principles to advanced use, building up each concept with both general examples and game AI examples.

---

## 1. What Is a State Machine?

A state machine is a model where a system is always in exactly one **state**, and transitions between states are driven by some logic that also carries **memory** — data that evolves as the machine runs.

Consider a door in a game world. It is either `Open`, `Closed`, or `Locked`. Those are its three states. When a player interacts with it, it transitions between them. The rules are the machine; the current state is where we are.

State machines are useful whenever behavior can be clearly expressed as:
- "Right now I am in state X."
- "Given some data, I should move to state Y."

They keep conditional logic organized and make it easy to reason about what a system is doing and why.

---

## 2. First Machine

```fsharp
open Prelude.Control

// State and memory types
type DoorState = Open | Closed | Locked | Done
type DoorMem   = { TimesOpened: int }

// Create a machine. "Done" is the exit state — the machine stops when it reaches it.
let door = StateMachineExec<DoorState, DoorMem>(Done)
```

`StateMachineExec<'state, 'mem>` takes your state type and memory type as type parameters, plus the exit state as a constructor argument. The machine stops and reports `ReachedExit` when any transition targets `Done`.

### Registering Handlers

A handler is a function `'mem -> TransitionMsg<'state, 'mem>`. It receives the current memory, does whatever it needs to do, and returns the next state and updated memory together.

```fsharp
door.Register(Closed, fun mem ->
    { NextState = Open; Mem = { mem with TimesOpened = mem.TimesOpened + 1 } })

door.Register(Open, fun mem ->
    { NextState = Done; Mem = mem })
```

### Running the Machine

Pass a `TransitionMsg` as the starting transition — the state to enter and the initial memory:

```fsharp
let status =
    door.Run({ NextState = Closed; Mem = { TimesOpened = 0 } })

// status.StopReason = ReachedExit { NextState = Done; Mem = { TimesOpened = 1 } }
```

The machine entered `Closed`, ran its handler, got `Open`, ran that handler, got `Done`, and stopped. `Run` returned a `MachineStatus` describing how it ended.

### Reading the Result

```fsharp
match status.StopReason with
| ReachedExit transition ->
    printfn "Door opened %d time(s)" transition.Mem.TimesOpened
| _ ->
    printfn "Unexpected stop"
```

---

## 3. A Game AI Example — Flat Combat FSM

Here is an NPC that cycles through simple combat states. The machine selects the next action based on health:

```fsharp
type CombatState = Assess | Attack | Flee | Rest | Done
type CombatMem   = { Health: int; EnemyVisible: bool }

let combat = StateMachineExec<CombatState, CombatMem>(Done)

combat.Register(Assess, fun mem ->
    if not mem.EnemyVisible then
        { NextState = Rest; Mem = mem }
    elif mem.Health < 30 then
        { NextState = Flee; Mem = mem }
    else
        { NextState = Attack; Mem = mem })

combat.Register(Attack, fun mem ->
    { NextState = Done; Mem = { mem with Health = mem.Health - 10 } })

combat.Register(Flee, fun mem ->
    { NextState = Done; Mem = mem })

combat.Register(Rest, fun mem ->
    { NextState = Done; Mem = { mem with Health = min 100 (mem.Health + 5) } })
```

Running it:

```fsharp
let result =
    combat.Run({ NextState = Assess; Mem = { Health = 20; EnemyVisible = true } })

// NPC was low health and an enemy was visible → transitioned to Flee → Done
```

`RunToStable` (the default) keeps advancing until the machine exits, yields, or waits. Each call to `Run` drives the machine from a given starting transition all the way to a terminal condition.

---

## 4. The `fsm { }` Computation Expression

Registering each handler as a plain lambda works fine, but there is also a computation expression that makes the handler body read more naturally — especially when the logic needs to read and update memory across a few steps:

```fsharp
combat.RegisterOutcome(Assess, fsm {
    let! mem = Fsm.getMem
    if not mem.EnemyVisible then
        return! Fsm.continueWith Rest
    elif mem.Health < 30 then
        return! Fsm.continueWith Flee
    else
        return! Fsm.continueWith Attack
})
```

The `fsm { }` block is syntactic sugar over `'mem -> StepOutcome`. Inside it:

| Operation | What it does |
|---|---|
| `Fsm.getMem` | Reads the current memory |
| `Fsm.setMem m` | Replaces memory with `m` |
| `Fsm.updateMem f` | Applies `f` to memory |
| `Fsm.continueWith s` | Transitions to `s` (keeps running) |
| `Fsm.yieldTo s` | Transitions to `s` (pauses here) |
| `Fsm.waitFor reason` | Stops and reports `reason` |

When reading and updating memory in the same handler:

```fsharp
combat.RegisterOutcome(Attack, fsm {
    do! Fsm.updateMem (fun m -> { m with Health = m.Health - 10 })
    return! Fsm.continueWith Done
})
```

The `fsm { }` CE is a convenience. It compiles to the same thing as a plain lambda — you can freely mix both styles in the same machine.

---

## 5. Pattern-Based Registration

When many states share the same logic, registering each one individually is repetitive. `RegisterCase` matches based on a discriminated union case extractor:

```fsharp
type EnemyState =
    | Patrol of patrolPoint: int
    | Chase
    | Attack
    | Done

type EnemyMem = { AlertLevel: float }

let enemy = StateMachineExec<EnemyState, EnemyMem>(Done)

// All Patrol states are handled by one rule
enemy.RegisterCase(
    (function Patrol p -> Some p | _ -> None),
    fun patrolPoint mem ->
        if mem.AlertLevel > 0.5 then
            { NextState = Chase; Mem = mem }
        else
            let next = (patrolPoint + 1) % 4
            { NextState = Patrol next; Mem = mem })

enemy.Register(Chase, fun mem ->
    { NextState = Attack; Mem = mem })

enemy.Register(Attack, fun mem ->
    { NextState = Done; Mem = mem })
```

There is also `RegisterWhen` when the condition is a predicate over the whole state:

```fsharp
// Any unhandled state with high alert goes to Chase
enemy.RegisterWhen(
    (fun _ -> true),
    fun mem -> { NextState = Chase; Mem = mem })
```

---

## 6. Execution Policies

By default `Run` advances the machine until it reaches the exit state. The execution policy controls this:

```fsharp
type ExecutionPolicy<'state, 'mem> =
    | RunToStable          // run until exit, yield, or wait (default)
    | MaxTransitions of n  // stop after n transitions
    | SingleStep           // take exactly one transition
    | Until predicate      // stop when predicate is true
```

These are useful when the machine is part of a simulation tick that cannot spend unbounded time:

```fsharp
// Run the NPC AI for at most 5 transitions this frame
let status = combat.RunFor({ NextState = Assess; Mem = mem }, maxTransitions = 5)

// If it didn't finish, resume next frame from where it stopped
match status.StopReason with
| TransitionBudgetReached ->
    // machine.CurrentConfig has the current state+mem for next frame
    ()
| ReachedExit _ -> ()
| _ -> ()
```

`StepCurrent` resumes from wherever the machine currently is without needing to pass the state and memory again:

```fsharp
// Next frame
let status2 = combat.StepCurrent()
```

---

## 7. Yield and Wait — The Steppable Machine

`StateMachineExec` is a wrapper that only uses `Continue`. When you need `Yield` (suspend and give control back to caller) or `Wait` (stop until an external event), use `SteppableStateMachineExec<'state, 'mem, 'wait>` directly:

```fsharp
type ScheduledAction =
    | SpeakLine of string
    | PlayAnimation of string
    | WaitForInput

type NpcState = Start | Speak | Animate | AwaitInput | Done
type NpcMem   = { Line: string; Anim: string }

let npc = SteppableStateMachineExec<NpcState, NpcMem, ScheduledAction>(Done)

npc.RegisterOutcome(Speak, fsm {
    let! mem = Fsm.getMem
    printfn "NPC says: %s" mem.Line
    // Yield: advance to Animate but suspend — let the caller drive timing
    return! Fsm.yieldTo Animate
})

npc.RegisterOutcome(Animate, fsm {
    let! mem = Fsm.getMem
    // Wait: stop entirely until caller says animation finished
    return! Fsm.waitFor (PlayAnimation mem.Anim)
})

npc.RegisterOutcome(AwaitInput, fsm {
    return! Fsm.waitFor WaitForInput
})
```

The three step outcomes:

| Outcome | Behavior |
|---|---|
| `Continue nextState` | Keep going immediately |
| `Yield nextState` | Stop now, leave a pending transition for the caller |
| `Wait reason` | Stop and expose `reason` so the caller knows what to wait for |

`Yield` is for cooperative frame slicing. `Wait` is for externally-driven resumption.

---

## 8. Pseudo-Parallel Composition

Behavior trees often have a **parallel** node that ticks multiple children during the same update and combines their results with a policy such as "all must succeed" or "any may succeed". `Prelude.Control` does not have a built-in behavior-tree runtime, but it can achieve the same gameplay effect cheaply by stepping multiple child machines once per frame and reducing their statuses.

`ParallelGroup` is the small coordinator for that pattern. Each `ParallelBranch` wraps a child machine, and the group combines branch states using a `ParallelPolicy`.

The direct non-CE surface is already clean enough to use on its own:

```fsharp
let moveBranch =
    ParallelBranch.singleStep
        "move"
        mover
        { NextState = Move; Mem = 3 }

let fireBranch =
    ParallelBranch.singleStep
        "fire"
        weapon
        { NextState = Fire; Mem = 2 }

let combatGroup =
    ParallelGroup.create AllMustSucceed [ moveBranch; fireBranch ]
```

If the group is hand-authored and you want it to read more like a small declaration, there is also a `parallelGroup { }` builder. It stays efficient because it only assembles the branches once up front; the per-frame execution is still just `ParallelGroup.tick` stepping the child branches.

```fsharp
open Prelude.Control

type MoveState = Move | MoveDone
type FireState = Fire | FireDone

let mover = StateMachineExec<MoveState, int>(MoveDone)
let weapon = StateMachineExec<FireState, int>(FireDone)

mover.Register(Move, fun stepsRemaining ->
    if stepsRemaining > 1 then
        { NextState = Move; Mem = stepsRemaining - 1 }
    else
        { NextState = MoveDone; Mem = 0 })

weapon.Register(Fire, fun shotsRemaining ->
    if shotsRemaining > 1 then
        { NextState = Fire; Mem = shotsRemaining - 1 }
    else
        { NextState = FireDone; Mem = 0 })

let combatGroupViaCe =
    parallelGroup {
        policy AllMustSucceed

        branch (ParallelBranch.singleStep
            "move"
            mover
            { NextState = Move; Mem = 3 })

        branch (ParallelBranch.singleStep
            "fire"
            weapon
            { NextState = Fire; Mem = 2 })
    }

for frame in 1 .. 3 do
    let tick = ParallelGroup.tick combatGroupViaCe
    printfn "frame %d -> %A, branches=%A" frame tick.GroupState tick.BranchStates

// frame 1 -> GroupRunning, branches=[|("move", Running); ("fire", Running)|]
// frame 2 -> GroupRunning, branches=[|("move", Running); ("fire", Succeeded)|]
// frame 3 -> GroupSucceeded, branches=[|("move", Succeeded); ("fire", Succeeded)|]
```

This is **pseudo-parallel**, not real thread parallelism. Both child machines are stepped on the same thread, one after the other, during the same frame. For most game AI uses, that is exactly what a behavior-tree parallel node is doing in practice: multiple concerns make progress during the same update, and a parent policy decides when the composite is done.

Use this pattern when you want combinations such as:
- move while firing
- aim while strafing
- follow a path while scanning for threats

Keep the outer control flow as an FSM or HFSM, and use `ParallelGroup` only inside the states that need this kind of composition.

---

## 9. Hierarchy — Shared Behavior Across States

Hierarchical state machines (HFSMs) let a group of states share a fallback handler. Any state in the group that does not have its own handler falls through to the group handler.

This is the most powerful tool for game AI. Consider an enemy with a dozen specific states. Nearly all of them share the same "check if dead" logic. Without hierarchy, every handler has to repeat that check. With hierarchy:

```fsharp
type EnemyState =
    | Idle | Patrol | Chase | Attack | Stagger | Dead | Done

type EnemyKey = AnyAlive | AttackGroup | IdleGroup | Done_

let enemy =
    OptimizedStateMachineExec<EnemyState, EnemyKey, EnemyMem>(Done, (function
        | Idle    -> IdleGroup
        | Patrol  -> IdleGroup
        | Chase   -> AnyAlive
        | Attack  -> AttackGroup
        | Stagger -> AttackGroup
        | Dead    -> Done_
        | Done    -> Done_))
```

The second constructor argument is the **hierarchy key** function — it maps each state to a group key. Then you register handlers at the group level:

```fsharp
// States Idle and Patrol are both in IdleGroup and will use this handler
enemy.RegisterHierarchy(IdleGroup, fun state mem ->
    if mem.EnemyVisible then
        { NextState = Chase; Mem = mem }
    else
        { NextState = Idle; Mem = mem })

// AttackGroup covers Chase, Attack, Stagger
enemy.RegisterHierarchy(AttackGroup, fun state mem ->
    if mem.Health <= 0 then
        { NextState = Dead; Mem = mem }
    else
        { NextState = Attack; Mem = mem })

// Override a specific state within the group (exact takes priority over hierarchy)
enemy.Register(Stagger, fun mem ->
    { NextState = if mem.Health > 10 then Chase else Dead; Mem = mem })
```

The resolution order is always: **exact state handler → group handler → parent group handler → …**

### Parent Chains

Groups can be nested. `RegisterParent` connects a child group to a parent:

```fsharp
enemy.RegisterParent(AttackGroup, AnyAlive)
enemy.RegisterParent(IdleGroup, AnyAlive)

// AnyAlive handles anything not caught by its child groups
enemy.RegisterHierarchy(AnyAlive, fun state mem ->
    if mem.Dead then
        { NextState = Done; Mem = mem }
    else
        { NextState = Idle; Mem = mem })
```

With this, the resolution for `Attack` is:
1. Exact handler for `Attack`? No.
2. Group handler for `AttackGroup`? Yes → use it.
3. (If not found, would walk up to `AnyAlive`.)

---

## 10. The Optimized Path — Many Instances, One Definition

`OptimizedStateMachineExec` works well for a single entity. When you have thousands of NPCs and each one is running the same behavior logic, creating a fresh machine per NPC wastes memory and CPU on redundant dispatch table construction.

`OptimizedStateMachine` separates the **compiled behavior definition** from the **per-instance runtime**:

```fsharp
// One compiled definition — built once, shared across all NPCs
let villagerBehavior : OptimizedStateMachine<VillagerState, VillagerKey, VillagerMem> =
    optimizedStateMachine {
        exit_state Done
        state_key villagerStateKey

        exact ChooseActivity (fun mem ->
            let next = pickActivityForHour mem.Hour mem.Schedule
            { NextState = Perform next; Mem = mem })

        children PerformKey [ SleepKey; WorkKey; RelaxKey ]

        hierarchy PerformKey (fun state mem ->
            match state with
            | Perform a -> { NextState = Done; Mem = { mem with Activity = Some a } }
            | _         -> invalidOp "unexpected")
    }

// Each NPC gets its own lightweight instance
let baker     = villagerBehavior.CreateInstance()
let nightGuard = villagerBehavior.CreateInstance()
```

The `optimizedStateMachine { }` CE compiles the dispatch tables once at definition time. The shared definition holds the tables. Each instance only holds the current state, memory, and a pending transition pointer.

### CE Operations

| Operation | What it declares |
|---|---|
| `exit_state s` | The exit sentinel state |
| `state_key f` | The hierarchy-key function |
| `exact s f` | Handler for one specific state |
| `exacts seq` | Handlers for many states at once |
| `hierarchy k f` | Fallback handler for a group |
| `parent childKey parentKey` | One parent/child relationship |
| `children parentKey [...]` | Many children of one parent at once |

Running an instance is the same as with any other machine:

```fsharp
let result = baker.Run({ NextState = ChooseActivity; Mem = initialMem })
```

---

## 11. Actor-Backed Execution

Every machine can optionally be driven through a `MailboxProcessor` actor, serializing all transitions through a message queue. This makes the machine safe to use across threads without external locking.

```fsharp
// Fire-and-forget: enqueue the transition, return immediately
machine.Post({ NextState = someState; Mem = mem })

// Synchronous: send the transition and block until the machine reports its status
let status = machine.RunOnActor({ NextState = someState; Mem = mem })
```

On the optimized path, the actor is lazy — it is not started until you first call `Post`, `RunOnActor`, or `StartActor`. Calling `Run` directly skips the actor entirely and runs synchronously on the calling thread.

```fsharp
// Explicitly start the actor early (e.g., on a background thread at startup)
let instance = villagerBehavior.CreateInstance(startActor = true)

// Later, from any thread
instance.Post({ NextState = ChooseActivity; Mem = mem })
```

When every NPC can be ticked synchronously (e.g., inside a game loop on one thread), skip actors entirely and just call `Run`. Actors add overhead; use them only when concurrent access is needed.

---

## 12. Observing State Changes

Every machine exposes a `MemStream` event that fires on every state entry with the current memory. This is useful for binding UI, logging, or other observers:

```fsharp
machine.MemStream.Add(fun mem ->
    printfn "[%s] health=%d" machine.CurrentState.Value mem.Health)
```

`CurrentState`, `CurrentMem`, and `CurrentConfig` are properties that reflect the live state between runs.

---

## 13. Choosing the Right API

| Need | Use |
|---|---|
| Simple flat FSM, synchronous | `StateMachineExec` |
| Yield / Wait behavior | `SteppableStateMachineExec` |
| Pseudo-parallel composition | `ParallelGroup` + child machines |
| One-off HFSM, mutable registration | `OptimizedStateMachineExec` |
| Many instances sharing one definition | `OptimizedStateMachine` + `CreateInstance` |
| Concurrent access from multiple threads | Any machine, drive it via `Post` / `RunOnActor` |

---

## 14. Putting It Together — Full NPC Schedule Example

This example shows the complete optimized path with per-NPC memory and a shared behavior definition:

```fsharp
open Prelude.Control

type NpcActivity = Sleep | Work | Relax

type NpcState =
    | ChooseNextActivity
    | Perform of NpcActivity
    | Finished

type NpcStateKey = ChooseKey | PerformKey | SleepKey | WorkKey | RelaxKey | FinishedKey

type DailySchedule =
    { SleepStart: int; SleepEnd: int
      WorkStart: int;  WorkEnd: int }

type NpcMemory =
    { Name: string
      Hour: int
      Schedule: DailySchedule
      CurrentActivity: NpcActivity option }

let hourInWindow s e h =
    if s <= e then h >= s && h < e else h >= s || h < e

let activityForHour sch h =
    if hourInWindow sch.SleepStart sch.SleepEnd h then Sleep
    elif hourInWindow sch.WorkStart sch.WorkEnd h then Work
    else Relax

let npcStateKey = function
    | ChooseNextActivity -> ChooseKey
    | Perform Sleep      -> SleepKey
    | Perform Work       -> WorkKey
    | Perform Relax      -> RelaxKey
    | Finished           -> FinishedKey

let villagerBehavior =
    optimizedStateMachine {
        exit_state Finished
        state_key  npcStateKey

        exact ChooseNextActivity (fun mem ->
            { NextState = Perform (activityForHour mem.Schedule mem.Hour); Mem = mem })

        children PerformKey [ SleepKey; WorkKey; RelaxKey ]

        hierarchy PerformKey (fun state mem ->
            match state with
            | Perform a -> { NextState = Finished; Mem = { mem with CurrentActivity = Some a } }
            | _         -> invalidOp "unexpected")
    }

let runNpc hour name schedule =
    let mem = { Name = name; Hour = hour; Schedule = schedule; CurrentActivity = None }
    let instance = villagerBehavior.CreateInstance()
    let status = instance.Run({ NextState = ChooseNextActivity; Mem = mem })
    match status.StopReason with
    | ReachedExit t ->
        printfn "%s at %02d:00 → %A" t.Mem.Name t.Mem.Hour t.Mem.CurrentActivity.Value
    | r -> failwithf "unexpected: %A" r

let bakerSchedule = { SleepStart = 21; SleepEnd = 5; WorkStart = 6; WorkEnd = 14 }
let guardSchedule = { SleepStart = 3; SleepEnd = 11; WorkStart = 18; WorkEnd = 2 }

runNpc 7  "Baker"       bakerSchedule   // Baker at 07:00 → Work
runNpc 20 "Baker"       bakerSchedule   // Baker at 20:00 → Relax
runNpc 7  "Night guard" guardSchedule   // Night guard at 07:00 → Sleep
runNpc 20 "Night guard" guardSchedule   // Night guard at 20:00 → Work
```

One `villagerBehavior` definition is compiled once. Each `CreateInstance()` call creates a lightweight per-NPC instance that holds only its own state and memory. At 10,000 NPCs, the dispatch tables are not duplicated — only the per-NPC runtime is.

---

## Summary

| Concept | One-liner |
|---|---|
| `StateMachineExec` | A flat FSM with named states and carried memory |
| `SteppableStateMachineExec` | Adds `Yield` (suspend) and `Wait` (external pause) |
| `ParallelGroup` | Steps several child machines per frame and combines them with a policy |
| `OptimizedStateMachineExec` | Flat HFSM with hierarchy keys, single instance |
| `OptimizedStateMachine` | Compiled shared definition + `CreateInstance()` for many instances |
| `fsm { }` | Computation expression for writing handlers with readable memory access |
| `optimizedStateMachine { }` | Computation expression for building shared definitions |
| `ExecutionPolicy` | Controls how far to run per call: full, bounded, single-step |
| Actor (`Post` / `RunOnActor`) | Optional thread-safe ownership boundary |
