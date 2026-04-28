# Prelude.Control — State Machine

`Prelude.Control` is an F# state machine library supporting flat and hierarchical finite state machines (HFSMs). It provides a memory-carrying transition model, multiple execution policies, optional actor-backed thread safety, and a specialized high-performance path for workloads where many machine instances share the same behavior definition.

## What Is Distinct About This Design

The design combines a few ideas that are often split across different libraries:

- **named control states plus explicit memory** rather than state-only transitions or hidden mutable state objects
- **hierarchical fallback dispatch** without requiring a full active-parent statechart runtime
- **one execution engine with multiple scheduling modes**: run-to-completion, bounded stepping, yielding, waiting, and actor-backed serialization
- **optional actor ownership** so the same machine model can be used either as a fast direct stepper or as a serialized message-driven component
- **a definition/runtime split on the optimized path** so one compiled HFSM can be reused across many live instances

The main result is a machine model that stays close to ordinary FSM control flow while remaining expressive enough for richer runtime behavior. States describe control structure. Memory carries evolving context. Hierarchy removes duplication. Execution policy controls how aggressively the machine advances. Actors provide an optional ownership boundary for concurrent use. The optimized path makes that same model practical at large instance counts.

---

## Quick Start

```fsharp
open Prelude.Control

let machine = StateMachineExec<string, int>("Done")

machine.Register("Start", fun count ->
    { NextState = "Finish"; Mem = count + 1 })

machine.Register("Finish", fun count ->
    { NextState = "Done"; Mem = count + 1 })

let status = machine.Run({ NextState = "Start"; Mem = 0 })
// status.StopReason = ReachedExit { NextState = "Done"; Mem = 2 }
```

---

## Core Model

### Transitions carry memory

There is no global mutable state. Each transition carries the next state and updated memory together:

```fsharp
type TransitionMsg<'state, 'mem> =
    { NextState: 'state
      Mem: 'mem }
```

The machine always receives a transition, updates its live configuration to that transition, and then resolves the next handler from the new state.

### The exit state is terminal

Every machine is constructed with an exit state. When a transition targets that state, the machine stops and reports `ReachedExit`. No handler is registered for the exit state — it is purely a sentinel.

### Step outcomes

The generic steppable API can express more than just "continue immediately":

```fsharp
type StepOutcome<'state, 'mem, 'wait> =
    | Continue of nextTransition: TransitionMsg<'state, 'mem>
    | Yield    of nextTransition: TransitionMsg<'state, 'mem>
    | Wait     of waitReason: 'wait
```

- `Continue` means keep stepping in the same run.
- `Yield` means stop now, but preserve a pending transition so the caller can resume later.
- `Wait` means stop and report an explicit external wait reason.

`Yield` and `Wait` are only available in `SteppableStateMachineExec`. The simpler `StateMachineExec` wrapper uses `Continue` exclusively. See [Choosing an API](#choosing-an-api).

### Execution policies

Execution is controlled by a policy rather than being hard-wired to "run until exit":

```fsharp
type ExecutionPolicy<'state, 'mem> =
    | RunToStable        // keep going until exit, Yield, or Wait (default)
    | MaxTransitions of int
    | SingleStep
    | Until of (('state * 'mem) option -> bool)
```

- **`RunToStable`** — run to completion. This is the default for all `Run` calls.
- **`MaxTransitions n`** — stop after at most `n` transitions. Useful for frame-budgeted AI or simulation ticks where unbounded execution per tick is unacceptable.
- **`SingleStep`** — stop after the first transition. Useful for debuggers and test harnesses that need to observe every step individually.
- **`Until predicate`** — stop when the predicate returns true for the candidate next config.

### Execution status

Every `Run` call returns:

```fsharp
type MachineStatus<'state, 'mem, 'wait> =
    { CurrentConfig: ('state * 'mem) option
      PendingTransition: TransitionMsg<'state, 'mem> option
      TransitionsProcessed: int
      StopReason: ExecutionStopReason<'state, 'mem, 'wait> }
```

| StopReason | Meaning |
| --- | --- |
| `ReachedExit` | Reached the designated exit state |
| `Yielded` | Handler returned `Yield`; `PendingTransition` holds the next step |
| `Waiting` | Handler returned `Wait` with a reason value |
| `TransitionBudgetReached` | Step budget (`MaxTransitions` or `SingleStep`) was exhausted |
| `PredicateSatisfied` | An `Until` predicate returned true |
| `NoCurrentConfig` | A `Step*` method was called with no current state |

---

## Actors: When to Use Them and When Not To

Every machine type includes an optional actor backend. Understanding when to use it matters more than knowing that it exists.

### What an actor does

The actor wraps the machine in a `MailboxProcessor` and gives that machine instance a single serialized owner.

When `Run` is used, the machine advances immediately on the caller's thread.

When `Post` or `RunOnActor` is used, the transition request is enqueued into the mailbox instead. The actor dequeues requests one at a time and runs the same internal stepping engine in mailbox order.

That gives the actor path two core properties:

- **serialization**: only one request mutates the machine at a time
- **ownership**: callers do not touch the machine directly; they hand work to the mailbox

The actor is therefore not a different state machine implementation. It is a concurrency adapter over the same stepping behavior.

### Actor API semantics

The three execution styles have different semantics:

| API | Runs on | Caller waits? | Main purpose |
| --- | --- | --- | --- |
| `Run` | caller thread | yes | cheapest synchronous execution |
| `RunOnActor` | mailbox thread | yes | serialized execution with a completion/result |
| `Post` | mailbox thread | no | serialized fire-and-forget execution |

This distinction is the practical key to understanding actors in this library:

- use `Run` for direct local execution
- use `RunOnActor` when a shared machine must stay serialized but the caller still needs the resulting status
- use `Post` when the caller only needs to submit work and move on

### When actors are useful

Use the actor path when:

- **Multiple threads need to drive the same machine.** Without an actor, `Run` is not thread-safe. Concurrent calls from two threads will corrupt the machine state. The actor serializes them.
- **You want fire-and-forget posting** without waiting for the run to complete. `Post` returns immediately; the machine processes the message asynchronously in the background.
- **You model an entity that receives external events** and want the message-passing style without manual locking.
- **You want one place to own mutation.** The mailbox becomes the single write boundary for that machine instance, which simplifies reasoning in concurrent code.

```fsharp
// Thread A — posts and moves on immediately
machine.Post({ NextState = "Active"; Mem = payload })

// Thread B — posts and waits for completion, getting the result back
let status = machine.RunOnActor({ NextState = "Recover"; Mem = errorPayload })
```

### When not to use actors

- **Single-threaded loops.** If only one thread drives the machine, `Run` is cheaper and just as correct. The actor adds mailbox enqueue/dequeue and async scheduling overhead on every transition.
- **High-throughput simulation.** Actor dispatch involves inter-thread hand-offs. For tight loops processing hundreds of thousands of transitions per second, synchronous `Run` is substantially faster.
- **Many short-lived per-frame decisions.** If a machine runs to completion once per tick and is only ever touched by one thread, the actor is pure overhead with no benefit.
- **Single-owner update loops.** If one scheduler, simulation loop, or service thread already owns each instance, `Run` keeps the machine fast and simple; adding a mailbox per instance usually adds cost without adding correctness.

### Advantages of actor-backed execution

- **Thread-safe serialized access to one machine instance.** Multiple producers can submit transitions safely.
- **Natural event-driven integration.** External messages can be posted as they occur rather than coordinating locks around `Run`.
- **Clear ownership boundary.** Mutation happens inside one mailbox loop instead of across arbitrary callers.
- **Optional asynchronous workflow.** `Post` supports fire-and-forget submission, while `RunOnActor` preserves a request/reply pattern.

### Costs of actor-backed execution

- **Mailbox overhead.** Every request must be enqueued and dequeued before execution starts.
- **Scheduling overhead.** Execution moves to the mailbox thread rather than staying on the caller thread.
- **Extra latency.** Even when total work is small, the request must wait its turn in the queue.
- **Less raw throughput.** For hot loops, direct synchronous `Run` is typically faster.

### Ownership Rule

For any individual machine instance, choose one of these ownership models and stay consistent:

- **direct ownership**: one thread owns the instance and uses `Run`
- **actor ownership**: many threads may submit work, but the instance is only advanced through `Post` and `RunOnActor`

Mixing unconstrained direct `Run` calls with concurrent actor usage on the same instance defeats the purpose of the mailbox and reintroduces race conditions.

### Summary

| Scenario | Recommended |
| --- | --- |
| Single-threaded, throughput-sensitive | `Run` (synchronous) |
| High-volume simulation, game AI, per-NPC ticks | `Run` on each instance |
| Multi-threaded access to one shared machine | `RunOnActor` or `Post` |
| Fire-and-forget external events | `Post` |
| Need a reply or completion signal across threads | `RunOnActor` |

### Optimized path note

On the optimized path, actor startup is lazy. Creating an instance does not allocate a mailbox thread. If you never call `Post`, `RunOnActor`, or `StartActor()`, no thread is ever started. This makes it practical to create thousands of instances and only pay the actor cost for the subset that actually needs concurrent access.

---

## API Families

### `StateMachineExec<'state, 'mem>`

The general-purpose machine. Use this as the default starting point.

**Handler registration:**

```fsharp
machine.Register(state, fun mem -> { NextState = ...; Mem = ... })
```

**Hierarchical / DU-pattern fallback:**

```fsharp
// Fires for any concrete state matched by the tryMatch function.
// The resolved handler is cached per concrete state after first match.
machine.RegisterCase(
    (function
     | Working child -> Some child
     | _ -> None),
    fun child mem -> ...)
|> ignore

// Or a predicate form:
machine.RegisterWhen((fun state -> ...), fun mem -> ...)
|> ignore
```

**Execution:**

```fsharp
machine.Run(transition)                    // RunToStable (default)
machine.Run(transition, MaxTransitions 5)
machine.RunSingleStep(transition)
machine.RunUntil(transition, predicate)
machine.RunOnActor(transition)             // actor-serialized, waits for reply
machine.Post(transition)                   // actor, fire-and-forget
```

**Stepping from current state:**

```fsharp
machine.StepCurrent()     // resume from pending or current config
machine.StepSingle()
machine.StepFor(n)
machine.StepUntil(pred)
```

**Observation:**

```fsharp
machine.CurrentState
machine.CurrentMem
machine.CurrentConfig
machine.PendingTransition
machine.MemStream     // IEvent<'mem> — fires on every transition
```

---

### `SteppableStateMachineExec<'state, 'mem, 'wait>`

The full-feature variant. Adds `Yield` and `Wait` outcomes in addition to `Continue`.

Use `RegisterOutcome` instead of `Register` to return these:

```fsharp
machine.RegisterOutcome(
    Acquire,
    fun mem ->
        Yield { NextState = Pathfind; Mem = mem })
```

`Yield` is the key addition: the handler decides to pause and hand back a pending transition. The caller resumes on the next frame by calling `StepCurrent()` or `StepFor(n)`.

`StateMachineExec` is a wrapper over `SteppableStateMachineExec` with `'wait = unit`. If you do not need `Yield` or `Wait`, use `StateMachineExec` — it has the same engine with a simpler type surface.

#### `fsm` handler authoring

`SteppableStateMachineExec` also exposes a small computation expression for authoring step handlers that lower directly to `Continue`, `Yield`, and `Wait`.

The CE threads the machine memory explicitly and compiles to the same handler shape used by `RegisterOutcome`:

```fsharp
machine.RegisterOutcome(
    Acquire,
    fsm {
        do! Fsm.updateMem (fun mem -> { mem with Budget = mem.Budget - 1 })
        let! mem = Fsm.getMem

        if mem.Budget <= 0 then
            return Fsm.waitFor OutOfBudget
        elif mem.NeedsPause then
            return Fsm.yieldTo Pathfind
        else
            return Fsm.continueWith Move
    })
```

The available primitives are intentionally small:

- `Fsm.getMem`
- `Fsm.setMem nextMem`
- `Fsm.updateMem f`
- `Fsm.continueWith nextState`
- `Fsm.yieldTo nextState`
- `Fsm.waitFor waitReason`

This is authoring syntax over the existing stepper, not a general coroutine runtime. The generated handler still returns exactly one `StepOutcome` for the current step, and the machine still resumes through the normal pending-transition and stepping APIs.

---

## Hierarchical State Machines (HFSMs)

An HFSM lets concrete child states reuse broader parent behavior without forcing every child to duplicate the same transition logic.

In this library, HFSM means **hierarchical dispatch** rather than a separate runtime stack of active parent and child states. At any moment the machine still has **one current concrete state**. The hierarchy is used to answer this question:

"If this exact state has no dedicated handler, which broader handler should run instead?"

Compared with an HFSM runtime that keeps both parent and child states active, this model has a different set of capabilities and tradeoffs:

- this library does **not** keep multiple active ancestor states at once
- it does **not** have built-in entry/exit propagation across a parent stack
- it **does** let child states inherit shared behavior from a parent grouping
- it **does** let exact child handlers override the parent behavior cleanly

### How this differs from more typical HFSMs

Many HFSM libraries model hierarchy as a runtime structure: a machine may conceptually be in both `Combat` and `Combat.Strike`, with parent entry/exit hooks, event bubbling, history states, and sometimes parallel regions.

This library takes a narrower approach:

- there is one current concrete state
- hierarchy is used for fallback dispatch
- shared data lives in the machine memory value, not in separate active parent-state instances

That makes the model simpler and more explicit.

### HFSM Execution Model

The model is:

- a single-current-state machine
- with hierarchical fallback dispatch
- plus explicit memory carried through every transition

Execution is still driven by ordinary state transitions, but the current configuration is not just a state label. It is the pair:

- current state
- current memory

This keeps the control structure compact while allowing rich runtime behavior through memory.

### Control States and Memory

At the API level this remains a finite-state-machine style model: handlers transition from one named state to another.

But there is an important distinction between:

- the finite set of **control states** represented by `'state`
- the full **runtime configuration** represented by `('state * 'mem)`

If `'mem` is itself finite, the whole machine is still finite in the strict formal sense.

If `'mem` can grow arbitrarily, then the overall system is more expressive than a classical finite automaton because behavior depends on both control state and carried memory. In practice, that is the useful part of the design: named states describe control flow, while memory carries the evolving context real applications need.

The intended mental model is:

- **control-flow structure from states**
- **runtime detail from memory**

### Flexibility

The flexibility comes from not forcing every concern into the state hierarchy itself.

In many state machine systems, there is pressure to encode more and more detail as more and more nested states. In this design, only the control-flow shape needs to live in `'state`. Everything else can stay in `'mem`.

That means you can keep states focused on control meaning:

- `Idle`
- `Combat WindUp`
- `Combat Strike`
- `Dead`

while memory carries runtime detail such as:

- timers
- targets
- cooldowns
- patrol routes
- last-seen positions
- history-like bookmarks
- pending external facts

This usually produces a better separation of concerns:

- states answer "where are we in the behavior?"
- memory answers "what data is this behavior operating on right now?"

### Advantages of this approach

- **States stay meaningful.** You do not have to explode the state space just to represent data variations. `Combat Strike` can remain one state even if many different targets, timings, or weapon details exist.
- **Hierarchy stays useful instead of becoming overloaded.** Parent-child relationships express shared behavior, not every piece of data specialization.
- **Behavior is explicit and testable.** A handler receives memory and returns the next state plus next memory. That is straightforward to test, replay, and reason about.
- **Serialization and persistence are simpler.** Persisting a machine means persisting a state plus memory, not a graph of active state objects with hidden mutable fields.
- **Large-instance scenarios work well.** The optimized path can share compiled control logic while each instance carries only its own memory.
- **You can model many "statechart-like" features explicitly.** History, guards, timers, deferred work, and interruption logic can all be represented through state, memory, and execution policy rather than needing special runtime machinery.

### Statechart Mapping

A substantial subset of statechart-style patterns can be modeled here, but they are expressed explicitly rather than through a full built-in statechart runtime.

Patterns that map well:

- **hierarchical shared behavior** via fallback handlers
- **guards** via ordinary code inside handlers
- **history-like behavior** by storing prior substate or mode in memory
- **timers and cooldowns** through memory plus stepping policies or external time input
- **interruptions** by transitioning to an override state when memory or inputs demand it
- **frame-sliced or deferred progression** via `Yield`, `Wait`, and pending transitions

Patterns that are not first-class runtime features here:

- automatic parent entry/exit propagation
- orthogonal/parallel regions
- implicit event bubbling through an active ancestor stack
- built-in history pseudostates as dedicated language constructs

Many statechart patterns therefore remain available, but they appear as explicit state-and-memory logic rather than as implicit runtime semantics. The resulting behavior stays visible in ordinary code instead of being distributed across framework-specific lifecycle hooks.

### Cons and non-goals

- **No active parent stack.** If a workflow relies on parent entry/exit callbacks firing automatically as children change, this model does not provide that out of the box.
- **No implicit event bubbling semantics.** A parent handler is selected by dispatch fallback, not by a richer runtime event propagation model.
- **No built-in history or orthogonal regions.** If those concepts are needed, they have to be modeled explicitly in state and memory.
- **Some structure must move into memory.** In more object-oriented HFSM frameworks, parent states often hold their own local mutable data. Here that data belongs in the shared memory value instead.

### Memory model vs typical state-local storage

In many state machine frameworks, state-specific data is spread across state classes or nested active state objects. In this library, long-lived runtime data is carried through the explicit machine memory value:

```fsharp
type TransitionMsg<'state, 'mem> =
        { NextState: 'state
            Mem: 'mem }
```

That has a few consequences:

- the full runtime context is always visible in one place
- parent-style and child-style handlers operate over the same memory model
- persistence, replay, testing, and deterministic stepping are easier because the machine evolves by explicit value transitions
- if you want a parent concept like "combat session data" or "patrol route state", that data is usually modeled as a part of `'mem` rather than as hidden mutable fields on an active parent state

In practice, the tradeoff is: less implicit runtime machinery, more explicit state-and-memory modeling.

The usual pattern is:

1. register exact handlers for special-case child states
2. register a broader fallback handler for the parent state/category
3. let the machine dispatch exact first, then fall back to the parent behavior

### What hierarchy looks like in practice

Suppose an NPC can be in combat, and there are several combat substates:

```fsharp
type CombatState =
    | WindUp
    | Strike
    | Recover

type NpcState =
    | Idle
    | Combat of CombatState
    | Dead
```

You might want:

- `Combat WindUp` to have a special exact handler
- all other `Combat _` states to share a common combat fallback

That is exactly the shape the HFSM support is built for.

### Generic HFSM path: DU matching and matcher-based fallback

The generic machine resolves hierarchy through `RegisterCase` and `RegisterWhen`. This is a strong fit for discriminated unions because you can describe hierarchy with ordinary pattern matching rather than maintaining a separate hierarchy table.

```fsharp
type CombatState =
    | WindUp
    | Strike
    | Recover

type NpcState =
    | Idle
    | Combat of CombatState
    | Dead

let machine = StateMachineExec<NpcState, string list>(Dead)

// Exact child override
machine.Register(
    Combat WindUp,
    fun log ->
        { NextState = Combat Strike
          Mem = "special wind-up timing" :: log })

// Parent-level combat behavior shared by other combat children
machine.RegisterCase(
    (function
     | Combat child -> Some child
     | _ -> None),
    fun child log ->
        match child with
        | WindUp ->
            { NextState = Combat Strike
              Mem = "generic combat wind-up" :: log }
        | Strike ->
            { NextState = Combat Recover
              Mem = "resolve attack" :: log }
        | Recover ->
            { NextState = Idle
              Mem = "return to idle" :: log })
|> ignore
```

#### Generic dispatch order

For `StateMachineExec` and `SteppableStateMachineExec`, hierarchical resolution works like this:

1. look for an exact state handler registered with `Register` or `RegisterOutcome`
2. if none exists, look for a previously cached fallback handler for that exact concrete state
3. if none is cached, scan the registered matcher handlers in registration order
4. cache the resolved fallback for that concrete state so the next visit is fast

That yields two important rules:

- exact child handlers always win over fallback handlers
- when multiple fallback matchers could match, **registration order matters**

So if you have overlapping `RegisterWhen` or `RegisterCase` handlers, register the more specific one first.

#### `RegisterCase` vs `RegisterWhen`

Use `RegisterCase` when the hierarchy is naturally expressed as DU case extraction:

```fsharp
machine.RegisterCase(
    (function Combat child -> Some child | _ -> None),
    fun child mem -> ...)
|> ignore
```

Use `RegisterWhen` when the grouping is easier to express as a predicate:

```fsharp
machine.RegisterWhen(
    ((=) Idle),
    fun mem -> ...)
|> ignore
```

`RegisterCase` is usually the better HFSM tool because it keeps the parent/child relationship explicit in the type shape rather than hiding it behind boolean predicates.

#### Generic HFSM strengths

- very ergonomic for DU-based state models
- no extra hierarchy type needed
- easy to add ad hoc parent behaviors
- good default choice when flexibility matters more than absolute throughput

#### Generic HFSM tradeoffs

- fallback resolution starts with matcher scanning rather than direct parent lookup
- overlapping matchers can become harder to reason about if registration order is unclear
- it is better suited to expressive modeling than to the most performance-sensitive large-instance workloads

### Optimized HFSM path: explicit hierarchy keys and parent chains

When the hierarchy structure is known ahead of time, the optimized path uses an explicit hierarchy key model instead of matcher scanning.

This separates two concepts that are often merged in simpler designs:

- the **concrete state**: what the machine is actually in right now
- the **hierarchy key**: which parent/category that state belongs to for fallback purposes

For example:

```fsharp
type CombatState =
    | WindUp
    | Strike
    | Recover

type NpcState =
    | Idle
    | Combat of CombatState
    | Dead

type NpcKey =
    | IdleKey
    | CombatKey
    | WindUpKey
    | StrikeKey
    | RecoverKey
    | DeadKey
```

The state-to-key mapping might be:

```fsharp
let toKey = function
    | Idle -> IdleKey
    | Combat WindUp -> WindUpKey
    | Combat Strike -> StrikeKey
    | Combat Recover -> RecoverKey
    | Dead -> DeadKey
```

And the parent relationships might be:

- `WindUpKey -> CombatKey`
- `StrikeKey -> CombatKey`
- `RecoverKey -> CombatKey`

That means all combat substates inherit the `CombatKey` fallback unless they have an exact override.

#### Optimized dispatch order

For `OptimizedStateMachineExec` and `OptimizedStateMachine`, resolution works like this:

1. look for an exact state handler
2. if none exists, look for a cached resolved hierarchy handler for that concrete state
3. if none is cached, map the state to its hierarchy key
4. try a handler for that key
5. if none exists, climb the registered parent chain until a handler is found
6. cache that resolved handler for the concrete state

This is more predictable than matcher scanning because the fallback route is an explicit parent chain rather than an open-ended sequence of predicates.

#### Manual optimized HFSM registration

With the low-level API, hierarchy is registered explicitly:

```fsharp
let machine = OptimizedStateMachineExec<NpcState, NpcKey, string list>(Dead, toKey)

machine.Register(
    Combat WindUp,
    fun log ->
        { NextState = Combat Strike
          Mem = "special wind-up timing" :: log })

machine.RegisterHierarchy(
    CombatKey,
    fun state log ->
        match state with
        | Combat Strike ->
            { NextState = Combat Recover
              Mem = "resolve attack" :: log }
        | Combat Recover ->
            { NextState = Idle
              Mem = "return to idle" :: log }
        | Combat WindUp ->
            { NextState = Combat Strike
              Mem = "generic wind-up" :: log }
        | _ ->
            invalidOp (sprintf "Unexpected combat state: %A" state))

machine.RegisterParent(WindUpKey, CombatKey)
machine.RegisterParent(StrikeKey, CombatKey)
machine.RegisterParent(RecoverKey, CombatKey)
```

The CE builder is just a more concise way to define the same structure.

#### What `parent` and `children` mean

In the optimized CE:

- `parent childKey parentKey` adds one parent link
- `children parentKey [ ... ]` adds the same parent link for several child keys

They do not create states or handlers on their own. They only define the fallback chain the dispatcher climbs when an exact handler is missing.

#### Optimized HFSM strengths

- explicit and predictable fallback structure
- faster hierarchical lookup for repeated dispatch
- easier to share one compiled hierarchy across many instances
- best choice when many entities use the same HFSM shape

#### Optimized HFSM tradeoffs

- requires a separate hierarchy key model
- more upfront structure than the generic DU-matching path
- less ad hoc than `RegisterCase` when you are still discovering the shape of the state space

### Which HFSM style should you choose?

Choose the generic HFSM path when:

- the state model is DU-heavy and you want to lean on direct pattern matching
- hierarchy is mostly a modeling convenience
- you care more about simplicity and flexibility than about maximum throughput

Choose the optimized HFSM path when:

- the hierarchy is stable and known in advance
- many machine instances share the same hierarchy
- dispatch predictability and instance scale matter
- you want one compiled definition reused across a large set of runtimes

### Practical mental model

The easiest way to think about HFSMs in this library is:

- exact handlers describe "this one child state is special"
- hierarchy handlers describe "all children in this family behave like this unless overridden"
- the generic path discovers that family through DU matching or predicates
- the optimized path encodes that family explicitly through keys and parent links

---

## The Optimized Path

For workloads with many machine instances — game NPCs, simulations, agent fleets — the optimized path separates the **compiled definition** from the **per-instance runtime**.

### Why this matters

A conventional approach creates one independent machine per entity. Every entity pays the cost of storing its own copy of transition tables, hierarchy maps, and parent links. If 10,000 NPCs share the same behavior, that is 10,000 redundant copies of identical data.

The optimized path compiles the behavior once into an `OptimizedStateMachine` definition. Each instance then only carries its own live state, memory, pending transition, and optional actor. Creating thousands of instances from one compiled definition takes a few milliseconds.

### Building a shared definition

```fsharp
let villagerBehavior : OptimizedStateMachine<NpcState, NpcStateKey, NpcMemory> =
    optimizedStateMachine {
        exit_state Finished
        state_key npcStateKey

        exact ChooseNextActivity (fun mem ->
            let next = activityForHour mem.Schedule mem.Hour
            { NextState = Perform next; Mem = mem })

        children PerformKey [ SleepKey; WorkKey; RelaxKey ]

        hierarchy PerformKey (fun state mem ->
            match state with
            | Perform activity ->
                { NextState = Finished
                  Mem = { mem with CurrentActivity = Some activity } }
            | _ ->
                invalidOp (sprintf "Unexpected state: %A" state))
    }
```

### Creating instances

```fsharp
// Cheap — allocates only live state fields, reuses compiled dispatch tables
let baker      = villagerBehavior.CreateInstance()
let nightGuard = villagerBehavior.CreateInstance()
let merchant   = villagerBehavior.CreateInstance()
```

Each instance runs independently with its own memory. The behavior graph is shared. If an NPC also needs actor-backed concurrent access:

```fsharp
let actorBacked = villagerBehavior.CreateInstance(startActor = true)
```

Otherwise no mailbox thread is allocated.

---

## Computation Expression Reference

The `optimizedStateMachine { ... }` builder supports:

| Operation | Purpose |
| --- | --- |
| `exit_state s` | Declares the terminal state (required) |
| `state_key f` | The `'state -> 'hierarchyKey` mapping (required) |
| `exact state handler` | Registers an exact transition handler for one state |
| `exacts [ state, handler; ... ]` | Registers multiple exact handlers at once |
| `hierarchy key handler` | Registers a fallback handler for a hierarchy key |
| `parent childKey parentKey` | Links one child key to one parent key |
| `children parentKey [ ... ]` | Links multiple children to one parent key |

Both `exit_state` and `state_key` are required. A missing one raises an error when the CE expression is evaluated, not at dispatch time.

---

## Full Example: Frame-Sliced AI

`SteppableStateMachineExec` for work that must pause and resume across frames:

```fsharp
open Prelude.Control

type AIPhase = Sense | Plan | Act | Done

let agent = SteppableStateMachineExec<AIPhase, int, unit>(Done)

agent.RegisterOutcome(Sense, fun tick ->
    printfn "[%d] Sense" tick
    Yield { NextState = Plan; Mem = tick + 1 })   // pause here, resume next frame

agent.Register(Plan, fun tick ->
    printfn "[%d] Plan" tick
    { NextState = Act; Mem = tick + 1 })

agent.Register(Act, fun tick ->
    printfn "[%d] Act" tick
    { NextState = Done; Mem = tick + 1 })

// Frame 1: run until the first Yield
let frame1 = agent.RunSingleStep({ NextState = Sense; Mem = 0 })
// frame1.StopReason  = Yielded
// frame1.PendingTransition = Some { NextState = Plan; Mem = 1 }

// Frame 2: resume from the pending transition
let frame2 = agent.StepCurrent()
// frame2.StopReason = ReachedExit
```

---

## Full Example: NPC Schedules (Shared Definition)

One compiled behavior definition, two NPCs with different schedules in their own memory:

```fsharp
open Prelude.Control

type Activity = Sleep | Work | Relax
type NpcState = ChooseActivity | DoActivity of Activity | Done
type NpcKey   = ChooseKey | DoKey | SleepKey | WorkKey | RelaxKey | DoneKey

type Schedule = { WorkStart: int; WorkEnd: int; SleepStart: int; SleepEnd: int }
type NpcMem   = { Name: string; Hour: int; Schedule: Schedule; Activity: Activity option }

let toKey = function
    | ChooseActivity   -> ChooseKey
    | DoActivity Sleep -> SleepKey
    | DoActivity Work  -> WorkKey
    | DoActivity Relax -> RelaxKey
    | Done             -> DoneKey

let hourInRange startH endH h =
    if startH <= endH then h >= startH && h < endH
    else h >= startH || h < endH

let pickActivity (s: Schedule) h =
    if hourInRange s.WorkStart s.WorkEnd h then Work
    elif hourInRange s.SleepStart s.SleepEnd h then Sleep
    else Relax

// Compiled once, shared across all NPC instances
let npcBehavior =
    optimizedStateMachine {
        exit_state Done
        state_key toKey

        exact ChooseActivity (fun mem ->
            { NextState = DoActivity (pickActivity mem.Schedule mem.Hour); Mem = mem })

        children DoKey [ SleepKey; WorkKey; RelaxKey ]

        hierarchy DoKey (fun state mem ->
            match state with
            | DoActivity a -> { NextState = Done; Mem = { mem with Activity = Some a } }
            | _            -> invalidOp "unexpected")
    }

let run name hour schedule =
    let instance = npcBehavior.CreateInstance()
    let mem = { Name = name; Hour = hour; Schedule = schedule; Activity = None }
    let status = instance.Run({ NextState = ChooseActivity; Mem = mem })
    match status.StopReason with
    | ReachedExit t -> printfn "%s at %02d:00 -> %A" t.Mem.Name hour t.Mem.Activity
    | r             -> printfn "unexpected: %A" r

let baker = { WorkStart = 6; WorkEnd = 14; SleepStart = 21; SleepEnd = 5 }
let guard = { WorkStart = 18; WorkEnd = 2;  SleepStart = 3;  SleepEnd = 11 }

let morning   = 9   // 09:00 — baker mid-shift, guard asleep
let lateNight = 23  // 23:00 — guard mid-shift, baker asleep

run "Baker"       morning   baker   // Baker at 09:00 -> Some Work
run "Night Guard" morning   guard   // Night Guard at 09:00 -> Some Sleep
run "Baker"       lateNight baker   // Baker at 23:00 -> Some Sleep
run "Night Guard" lateNight guard   // Night Guard at 23:00 -> Some Work
```

---

## Choosing an API

```
Do many instances share the same behavior definition?
├─ Yes → optimizedStateMachine { ... } + CreateInstance()
└─ No  → Does the machine need to Yield or Wait mid-run?
          ├─ Yes → SteppableStateMachineExec
          └─ No  → StateMachineExec

Does any execution need to be thread-safe or fire-and-forget?
├─ Yes → Post / RunOnActor  (actor path)
└─ No  → Run                (synchronous, no overhead)
```

---

## Feature Summary

| Feature | `StateMachineExec` | `SteppableStateMachineExec` | Optimized |
| --- | :---: | :---: | :---: |
| Exact state handlers | ✓ | ✓ | ✓ |
| DU/predicate fallback | ✓ | ✓ | — |
| Explicit hierarchy keys | — | — | ✓ |
| Yield / Wait outcomes | — | ✓ | — |
| All execution policies | ✓ | ✓ | ✓ |
| Observable state and memory | ✓ | ✓ | ✓ |
| Memory event stream | ✓ | ✓ | ✓ |
| Actor-backed execution | ✓ | ✓ | ✓ |
| Lazy actor startup | — | — | ✓ |
| Shared compiled definition | — | — | ✓ |
| CE builder | — | — | ✓ |