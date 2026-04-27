# HFSM Optimization Plan

## Goal

Improve the performance of the current HFSM implementation in `Control.fs` without losing the useful ergonomics we already have:

- exact state handlers
- DU-friendly hierarchical handlers
- optional actor-based serialized execution
- observable current state and memory

The benchmark baseline already shows that the current actor-driven machine is materially slower than a direct synchronous loop, so optimization should focus on reducing per-transition overhead before attempting broader redesign.

## Benchmark History

Each benchmark result is stored as a row so later phases can append cleanly without adding more columns.

| Snapshot | Phase | Samples | Scenario | Avg | Median | P95 | P99 | Min | Max | Notes |
| --- | --- | ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | --- |
| Git snapshot | Pre-Phase 1 | 30 | Direct exact loop | 3.133 ms | 3.235 ms | 5.145 ms | 5.271 ms | 0.873 ms | 5.271 ms | Rerun with current `timeThisWithSetup` benchmark harness |
| Git snapshot | Pre-Phase 1 | 30 | Direct HFSM loop | 3.967 ms | 3.666 ms | 5.283 ms | 5.353 ms | 2.977 ms | 5.353 ms | Rerun with current `timeThisWithSetup` benchmark harness |
| Git snapshot | Pre-Phase 1 | 30 | Actor exact dispatch | 536.258 ms | 458.062 ms | 941.369 ms | 1047.945 ms | 374.112 ms | 1047.945 ms | Rerun with current `timeThisWithSetup` benchmark harness |
| Git snapshot | Pre-Phase 1 | 30 | Actor HFSM dispatch | 524.291 ms | 500.472 ms | 689.337 ms | 692.638 ms | 432.834 ms | 692.638 ms | Rerun with current `timeThisWithSetup` benchmark harness |
| Extracted snapshot | Phase 1 | 30 | Direct exact loop | 3.217 ms | 1.950 ms | 5.279 ms | 5.317 ms | 1.371 ms | 5.317 ms | Rerun from validated `phase1-temp.txt` with current `timeThisWithSetup` benchmark harness |
| Extracted snapshot | Phase 1 | 30 | Direct HFSM loop | 6.326 ms | 6.209 ms | 8.609 ms | 8.737 ms | 4.434 ms | 8.737 ms | Rerun from validated `phase1-temp.txt` with current `timeThisWithSetup` benchmark harness |
| Extracted snapshot | Phase 1 | 30 | Actor exact dispatch | 314.489 ms | 271.898 ms | 606.120 ms | 790.024 ms | 256.942 ms | 790.024 ms | Rerun from validated `phase1-temp.txt` with current `timeThisWithSetup` benchmark harness |
| Extracted snapshot | Phase 1 | 30 | Actor HFSM dispatch | 289.762 ms | 281.853 ms | 332.255 ms | 335.236 ms | 265.479 ms | 335.236 ms | Rerun from validated `phase1-temp.txt` with current `timeThisWithSetup` benchmark harness |
| Current source | Phase 2 | 30 | Direct exact loop | 2.635 ms | 3.214 ms | 3.902 ms | 4.276 ms | 0.713 ms | 4.276 ms | Verified after replacing mailbox self-posting with synchronous internal stepping |
| Current source | Phase 2 | 30 | Direct HFSM loop | 4.977 ms | 4.742 ms | 7.009 ms | 8.801 ms | 3.066 ms | 8.801 ms | Verified after replacing mailbox self-posting with synchronous internal stepping |
| Current source | Phase 2 | 30 | Actor exact dispatch | 119.874 ms | 94.419 ms | 189.152 ms | 199.406 ms | 82.636 ms | 199.406 ms | Verified after replacing mailbox self-posting with synchronous internal stepping |
| Current source | Phase 2 | 30 | Actor HFSM dispatch | 126.436 ms | 121.357 ms | 187.295 ms | 189.163 ms | 89.101 ms | 189.163 ms | Verified after replacing mailbox self-posting with synchronous internal stepping |
| Current source | Phase 2 + steppable API recovery | 30 | Direct exact loop | 3.702 ms | 4.371 ms | 5.825 ms | 6.003 ms | 0.721 ms | 6.003 ms | Recorded after the steppable execution API refactor and a hot-path recovery pass |
| Current source | Phase 2 + steppable API recovery | 30 | Direct HFSM loop | 5.314 ms | 4.942 ms | 7.737 ms | 10.367 ms | 3.452 ms | 10.367 ms | Recorded after the steppable execution API refactor and a hot-path recovery pass |
| Current source | Phase 2 + steppable API recovery | 30 | Actor exact dispatch | 150.809 ms | 123.891 ms | 234.668 ms | 247.231 ms | 98.067 ms | 247.231 ms | Regression note: the steppable API refactor pushed recent actor reruns up into the `~190-210 ms` range before a `RunToStable` fast-path recovery pass pulled exact dispatch back down materially |
| Current source | Phase 2 + steppable API recovery | 30 | Actor HFSM dispatch | 158.862 ms | 159.627 ms | 188.862 ms | 192.404 ms | 134.068 ms | 192.404 ms | Regression note: the steppable API refactor pushed recent actor reruns up into the `~190-210 ms` range before a `RunToStable` fast-path recovery pass pulled HFSM dispatch back down materially |
| Current source | Phase 3 first pass | 30 | Direct exact loop | 3.154 ms | 3.318 ms | 5.714 ms | 5.953 ms | 0.975 ms | 5.953 ms | Added a concrete-state hierarchical dispatch cache with cache-first fallback resolution |
| Current source | Phase 3 first pass | 30 | Direct HFSM loop | 4.937 ms | 4.759 ms | 8.128 ms | 9.314 ms | 2.958 ms | 9.314 ms | Added a concrete-state hierarchical dispatch cache with cache-first fallback resolution |
| Current source | Phase 3 first pass | 30 | Actor exact dispatch | 148.199 ms | 141.256 ms | 198.696 ms | 221.071 ms | 120.719 ms | 221.071 ms | Slightly improved versus the steppable recovery row, but still above the original Phase 2 exact-dispatch snapshot |
| Current source | Phase 3 first pass | 30 | Actor HFSM dispatch | 139.913 ms | 134.869 ms | 163.147 ms | 218.858 ms | 117.240 ms | 218.858 ms | Materially improved versus the steppable recovery row; first evidence that avoiding repeated matcher scans helps the HFSM path |

Archived early benchmark:

- before the 30-sample policy, an early rough baseline was captured at 6 samples: direct exact `187 ms`, direct HFSM `198 ms`, actor exact `941 ms`, actor HFSM `855 ms`
- those numbers are retained only as historical context and should not be used for phase-to-phase comparison now that a proper 30-sample pre-Phase-1 snapshot exists
- `phase1-temp.txt` matched the expected Phase 1 and has now been rerun under the same current timing harness
- a "Post-timing-fix baseline" was also captured immediately after the timing fix and before Phase 2, but it has been removed from the main table because the dedicated Pre-Phase-1 and Phase 1 reruns are the clearer authoritative comparisons

Interpretation:

- mailbox/self-post overhead is currently dominating more than hierarchical dispatch overhead
- the optimized path should target the hot transition loop first
- the actor should be treated as a concurrency boundary, not as the core optimization strategy
- the pre-Phase-1 snapshot has now been rerun with the current timing helper, so it is the correct baseline for any new apples-to-apples comparisons
- the extracted Phase 1 snapshot has now also been rerun with the current timing helper, so Pre-Phase-1 and Phase 1 are finally apples-to-apples
- relative to Pre-Phase-1, the Phase 1 rerun improved actor exact and actor HFSM dispatch materially
- the direct exact and direct HFSM loops are harness-control rows; they do not exercise `Control.fs`, so drift there should be treated as benchmark noise or runtime variation rather than a regression caused by Phase 1 or Phase 2 source changes
- the meaningful phase-to-phase signal is in the actor exact and actor HFSM rows, because those are the rows that actually execute the state-machine implementation under test
- the large apparent direct-loop drop from roughly `175 ms` to roughly `3 ms` is therefore a benchmarking-methodology correction, not a real algorithmic speedup in the direct loop itself
- the correct apples-to-apples comparisons now are Pre-Phase-1 vs Phase 1 under the corrected harness, and then Phase 1 vs Phase 2 if Phase 2 is rerun or otherwise interpreted with the same corrected-harness context in mind
- at `30` samples, `avg`, `median`, and `p95` are informative enough for iterative optimization work
- `p99` is still less stable than `p95`, but no longer collapses immediately into a trivial single-sample tail in the way it did at `12` samples
- the earlier apparent actor regression was likely caused by benchmarking an inconsistent build state or otherwise incomparable run conditions
- a valid apples-to-apples pre-Phase-1 baseline now exists in the table
- Phase 2 validated the main architectural hypothesis: mailbox self-posting was a dominant cost, and collapsing internal transitions into a synchronous stepper materially reduced actor overhead
- after Phase 2, actor exact and actor HFSM dispatch are much closer to each other, which suggests mailbox churn was a larger bottleneck than hierarchical matcher overhead in the previous design
- the later steppable execution API refactor introduced a real actor-path regression relative to the original Phase 2 row; a targeted `RunToStable` hot-path recovery pass improved that materially, but the current actor numbers still remain above the original `119.874 ms` / `126.436 ms` Phase 2 snapshot and should be treated as regression debt going into Phase 3
- the first Phase 3 pass improved actor HFSM materially relative to the steppable-recovery row (`158.862 ms` to `139.913 ms` avg), which supports the claim that repeated matcher scans are still meaningful overhead on the hierarchical path
- the same Phase 3 pass only slightly improved actor exact (`150.809 ms` to `148.199 ms` avg), so the remaining regression debt versus the original Phase 2 row is now more concentrated in the shared actor/stepper path than in hierarchical dispatch alone

Benchmark policy going forward:

- use `30` samples as the default benchmark count
- this is high enough to make `avg`, `median`, and `p95` materially more informative than a `6` or `12` sample run
- this still keeps the full four-scenario benchmark practical to rerun during iteration
- `p99` will remain coarse at `30` samples, so `p95` should be treated as the primary tail metric unless we later introduce a longer-running dedicated benchmark mode

## Optimization Principles

1. Optimize the measured hot path first.
2. Separate CPU-bound stepping from asynchronous coordination.
3. Prefer predictable dispatch and contiguous data over flexible but allocation-heavy matcher chains.
4. Keep the generic API, but move expensive flexibility off the hot path.
5. Re-benchmark after each phase rather than batching many changes together.

## Main Hotspots

These are the current likely costs in the implementation:

- per-step mailbox enqueue and dequeue
- per-step async loop machinery
- exact-hit closure wrapping in transition resolution
- linear scan through matcher registrations
- duplicate DU matching in `RegisterCase`
- unconditional event triggering on every transition

## Phased Plan

## Phase 1: Remove Easy Overhead

Purpose:

Reduce obvious allocations and unnecessary work without changing the architecture.

Changes:

- remove avoidable async allocation inside the transition loop
- avoid wrapping exact transition handlers in fresh closures on lookup
- make `RegisterCase` evaluate its DU extractor once per dispatch path
- keep the current public API shape where practical

Expected outcome:

- small but reliable reduction in per-transition overhead
- cleaner baseline before larger structural changes

Status after first pass:

- completed
- removed avoidable async allocation inside the transition loop
- removed exact-hit closure wrapping during transition resolution
- reduced matcher dispatch overhead with a tighter imperative resolver
- made `RegisterCase` resolve DU matches once per dispatch path
- benchmark results now include median, p95, and p99 in addition to avg, min, and max
- the extracted Phase 1 code has been rerun with the current timing harness and now has authoritative 30-sample rows in the history table

## Phase 2: Split Core Stepper from Actor Shell

Purpose:

Separate the fast state-transition engine from the mailbox runtime.

Changes:

- introduce a synchronous stepping core for internal transitions
- keep `MailboxProcessor` only as the outer serialized event boundary
- process internal transitions in a tight loop until a wait or terminal state is reached

Expected outcome:

- large reduction in actor overhead for internally-driven state progress
- preservation of actor ergonomics for external concurrency

Status after first pass:

- completed
- introduced a synchronous internal stepping loop inside `StateMachineExec`
- kept `MailboxProcessor` as the serialized outer boundary for externally posted transitions
- removed per-transition mailbox self-posting from the internal progression path
- a rebuilt 30-sample benchmark shows large actor-path improvements while preserving the existing public API shape

## Execution API Sketch

Goal:

Keep the Phase 2 synchronous stepping gains, but stop forcing one scheduling policy on every caller.

Core design idea:

- keep the fast synchronous internal stepper
- separate stepping from scheduling policy
- treat the actor wrapper as one adapter, not the only execution model

Recommended layering:

1. core stepper
2. execution policy layer
3. adapters for actor-driven, tick-driven, and debug-oriented usage

Possible result model:

```fsharp
type StepOutcome<'state, 'mem, 'wait> =
	| Transition of TransitionMsg<'state, 'mem>
	| Wait of 'wait
	| Yield
	| Stop of 'mem
```

Possible policy model:

```fsharp
type ExecutionPolicy<'state, 'mem> =
	| RunToStable
	| MaxTransitions of int
	| SingleStep
	| Until of (('state * 'mem) option -> bool)
```

Possible status/snapshot model:

```fsharp
type MachineStatus<'state, 'mem, 'wait> =
	{ Config: ('state * 'mem) option
	  TransitionsProcessed: int
	  Outcome: StepOutcome<'state, 'mem, 'wait> option }
```

Possible engine surface:

```fsharp
member Step : ExecutionPolicy<'state, 'mem> -> MachineStatus<'state, 'mem, 'wait>
member Resume : 'event -> MachineStatus<'state, 'mem, 'wait>
member CurrentConfig : ('state * 'mem) option
```

Intended usage modes:

- `RunToStable` for throughput-oriented workflows and the current Phase 2 behavior
- `MaxTransitions n` for game AI or frame-budgeted simulation
- `SingleStep` for debugging, tooling, and deterministic inspection
- `Until predicate` for custom stop conditions
- `Wait` and `Yield` so the machine can suspend explicitly instead of encoding suspension as more immediate transitions

Why this preserves Phase 2 gains:

- the synchronous internal stepping loop remains the engine
- the expensive mailbox-per-transition pattern stays gone
- only the stopping condition becomes configurable

Why this improves UX/flexibility:

- callers can cap work per tick or frame
- external-event-driven workflows still get a run-to-stable mode
- debugging becomes much easier with explicit single-step behavior
- game AI can express "wait until next tick/event" directly instead of abusing state transitions for suspension

Compatibility direction:

- keep the current `StateMachineExec` surface as the default `RunToStable` wrapper
- add the more explicit stepping API underneath or alongside it
- keep the actor wrapper as an adapter over the same core rather than as the core itself

## Phase 3: Introduce Fast Hierarchical Dispatch

Purpose:

Replace generic predicate scanning as the primary HFSM dispatch strategy.

Changes:

- define an explicit dispatch-key model for state hierarchy
- resolve exact handler first, then parent chain fallback
- use precomputed hierarchy information instead of repeatedly scanning matcher predicates

Options:

- dictionary-backed exact and parent lookup for a generic implementation
- array-backed dispatch if state keys can be mapped to dense integers

Expected outcome:

- much better cache locality
- more predictable branch behavior
- lower overhead for DU-based hierarchical dispatch

Status after first pass:

- in progress
- added a concrete-state fallback cache for hierarchical dispatch resolution
- changed hierarchical matcher resolution to cache concrete-state handlers and prefer the cache on repeated fallback hits
- benchmarked improvement on actor HFSM dispatch relative to the steppable-recovery row, while actor exact remains close to that row and still above the original Phase 2 snapshot

Rejected follow-up experiments:

- rejected a dedicated single-matcher fast path layered on top of the cache-first fallback design; it compiled after local repair but benchmark reruns moved actor HFSM away from the retained Phase 3 first-pass result instead of improving it
- rejected an explicit keyed-dispatch registration API as a benchmarked replacement for `RegisterCase`; after resolving the F# signature/inference issues, the keyed path still underperformed the retained Phase 3 first-pass benchmark (`148.745 ms` then `162.197 ms` actor HFSM avg versus the retained `139.913 ms` row)
- rejected prepopulating the resolved dispatch cache with exact handlers; this regressed both actor exact and actor HFSM (`155.369 ms` / `163.587 ms` avg versus the restored `~151 ms` / `~151 ms` snapshot)
- rejected switching the fallback cache to `DictionarySlim`; it would have required strengthening the public `'state` constraint to `IEquatable<'state>`, so it was not acceptable as a drop-in optimization for the current API shape
- rejected an explicit parent-chain fallback API (`SetParentResolver` plus `RegisterParent`) as the final Phase 3 experiment; the benchmarked parent-chain HFSM path was not better than the retained matcher-cache path (`183.857 ms` actor HFSM avg versus `183.971 ms` for the side-by-side case-dispatch control in that run), so the experiment was backed out

Current Phase 3 takeaway:

- the retained cache-first fallback resolution remains the best Phase 3 result so far
- the obvious low-risk micro-optimizations around the current matcher model have now mostly been explored and logged
- the last structural Phase 3 experiment, explicit parent-chain fallback, also failed to beat the retained matcher-cache path in the benchmarked HFSM scenario
- Phase 3 should therefore be treated as exhausted for now; the next iteration should move to Phase 4 rather than continue piling on more dispatch experiments around the current matcher model

## Phase 4: Make Observability Optional

Purpose:

Keep diagnostics without forcing every hot run to pay for them.

Changes:

- make event publication optional or conditional
- isolate instrumentation from the core stepping path

Expected outcome:

- lower overhead in benchmark and production hot paths
- diagnostics remain available when needed

Status after first pass:

- attempted and rejected for now
- tried making `MemStream` publication opt-in by default so `msgEvent.Trigger` would only run when observability was explicitly enabled or the stream was accessed
- benchmark reruns during that experiment were materially worse than the best retained pre-Phase-4 snapshot, so the change was backed out
- after backing it out, reruns still did not immediately return to the unusually low `117.957 ms` / `123.476 ms` actor snapshot, so that exact low run should be treated as a favorable benchmark point rather than as a guaranteed steady-state baseline for every rerun

Current Phase 4 takeaway:

- the naive per-transition branch around `msgEvent.Trigger` did not produce a measurable improvement in the benchmarked path and may have been a net loss
- if observability is revisited, it should be done with a design that removes or relocates the instrumentation cost without adding another hot-path branch to every transition

## Phase 5: Specialize for High-Performance Scenarios

Purpose:

Add a more specialized path for cases where throughput matters more than maximum flexibility.

Changes:

- offer a specialized HFSM builder using explicit hierarchy keys
- optionally support integer dispatch ids for array-based lookup
- keep the existing flexible matcher API as a slower compatibility layer

Expected outcome:

- best possible throughput for stable state-machine shapes
- a clear distinction between generic and optimized modes

Status after first pass:

- added `OptimizedStateMachineExec<'state,'hierarchyKey,'mem>` in `Control.fs`
- added `optimizedStateMachine { ... }` computation-expression builder for explicit hierarchy-key registration in the same file
- the optimized path keeps exact handlers separate from hierarchy handlers and resolves hierarchy fallbacks through an explicit parent-key chain instead of matcher scans
- first benchmark pass with the actor wrapper showed a material improvement on the exact path and a smaller but still meaningful improvement on the HFSM path
- added `RunOnActor` so benchmarks can wait for full actor-side completion instead of signaling completion from inside a transition function
- added optimized synchronous-core benchmark rows for both exact and HFSM paths
- productized the public Phase 5 surface so `optimizedStateMachine { ... }` now builds a reusable `OptimizedStateMachine<'state,'hierarchyKey,'mem>` definition with `CreateInstance(?startActor)`, instead of only returning a benchmark-oriented mutable exec object

Initial Phase 5 benchmark snapshot:

- actor exact: `165.506 ms`
- actor HFSM: `155.989 ms`
- optimized actor exact: `85.081 ms`
- optimized actor HFSM: `113.076 ms`

Current Phase 5 takeaway:

- the specialized builder/path is justified: exact dispatch improved substantially versus the current generic actor path
- explicit parent-key fallback also improved the hierarchical path, though the gain is smaller than the exact-path win
- the optimized synchronous-core run now gives the missing comparison point, and reruns show the expected pattern: optimized sync exact is faster than optimized actor exact, while optimized HFSM actor/sync rows are close enough that normal benchmark variance still matters

Current validated Phase 5 benchmark picture:

- actor exact: `154.246 ms`
- actor HFSM: `148.845 ms`
- optimized actor exact: `83.747 ms`
- optimized actor HFSM: `112.234 ms`
- optimized sync exact: `72.657 ms`
- optimized sync HFSM: `117.547 ms`

Cleanup/validation follow-up:

- `hfsm_bench.fsx` now shares optimized-machine setup helpers so the Phase 5 examples read as one coherent usage instead of four repeated setup blocks
- benchmark reruns after the cleanup pass stayed within the same overall range; latest rerun was actor exact `141.093 ms`, actor HFSM `144.698 ms`, optimized actor exact `76.793 ms`, optimized actor HFSM `114.198 ms`, optimized sync exact `71.153 ms`, optimized sync HFSM `128.467 ms`

Phase 5 completion status:

- functionally, this phase is complete for the current objective
- the specialized optimized core exists, the CE surface is usable, and we now have both actor and synchronous benchmark rows with corrected actor-side completion measurement
- any further work here is refinement or productization rather than a missing foundational piece

Recommended close-out decision:

- treat Phase 5 as complete and move on unless a new benchmark-driven bottleneck appears

## Data Structure Direction

Preferred direction for the optimized path:

- synchronous tight stepping loop for internal transitions
- dictionary or array lookup for exact handlers
- explicit parent-chain fallback instead of matcher scanning
- minimal allocations in the transition loop
- optional actor wrapper around the optimized core

Less preferred in the hot path:

- repeated predicate scans
- per-step closure creation
- per-step async wrappers
- mailbox self-posting for every internal transition

## Measurement Strategy

After each phase, compare at least:

- direct exact loop
- direct hierarchical loop
- current actor path
- optimized actor-wrapped path
- optimized synchronous core path

Measure:

- average runtime
- min and max runtime
- transitions per second
- allocation behavior if we add allocation measurement later

## Success Criteria

The optimization work is successful if we achieve most of the following:

- significantly lower actor-based transition cost than the current baseline
- hierarchical dispatch overhead stays close to exact dispatch overhead
- the synchronous core approaches the direct loop benchmark more closely than the current implementation
- API remains understandable for DU-based HFSM usage

## Immediate Next Step

Add a direct synchronous benchmark for the optimized Phase 5 core.

The actor-wrapped optimized path is already materially better than the generic actor path, so the next discriminating check is how much of the remaining cost is still mailbox overhead versus core dispatch/transition overhead.