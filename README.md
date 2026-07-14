# Prelude

A set of useful F# utility functions and data structures. Available as a NuGet package targeting **net5**.

**Author:** Deen Abiola  
**Version:** 2.1.0  
**Package ID:** `Prelude`

---

## Installation

```
dotnet add package Prelude
```

Or via GitHub Packages (owner: `sir-deenicus`).

---

## Modules

### `Prelude.Common`
Core utilities that underpin the rest of the library.

- `MaybeBuilder` — computation expression for `option`-chaining
- `Either<'a,'b>` — discriminated union for two-case branching
- `fallibleComputation` / `lazyFallibleComputation` — safe `Result`-wrapped exception handling
- Active patterns: `IsDateTime`, `IsNumber`, `JustString`, `ToFloat`, `ToInt`, `ToArray`, etc.
- String-to-numeric converters: `toDouble`, `toInt`
- Collection helpers and common type aliases (`MutableList`, `Hashset`, `Dict`)

---

### `Prelude.Math`
Mathematical utilities and statistics.

- Basic math: `pow`, `squared`, `log2`, `nearestPow2`, `isPowerOf2`
- `Stats` submodule:
  - `SummaryStats` record (mean, stddev, min, max, median, …)
  - `simpleStats` — slope, intercept, covariance, variance for two sequences
  - `pearsonsCorr` — Pearson correlation coefficient
  - `online_mean` — incremental mean update

---

### `Prelude.Onlinelearning`
Lightweight online machine learning primitives.

- `regress` / `regressAvg` — gradient-descent regression step with averaged weights
- `averagedPerceptronStep` — averaged perceptron update
- `rootMeanError` — RMSE helper

---

### `Prelude.Reducers`
F# implementation of Clojure-style parallel reducers (based on Nick Palladinos' `fssnip.net/ip`).

- `toSeqReducer` — sequential reducer over any `seq<'T>`
- `toParallelReducer` — async parallel reducer over arrays, with configurable sequential-reduce threshold
- Composable `ReduceFunc` / `CombineFunc` abstractions

---

### `Prelude.Collections.FibonacciHeap`
A Fibonacci heap for use in priority-queue-sensitive algorithms (e.g., Dijkstra with decrease-key).

---

### `Prelude.SimpleGraphs` / `Prelude.SimpleDirectedGraphs`
Lightweight in-memory graph types.

- Undirected and directed weighted/unweighted graphs
- `IWeightedGraph<'node, 'weight>` interface
- `RawEdgeWeightData` returning either node-keyed or edge-keyed weight dictionaries

---

### `Prelude.GraphAlgorithms`
Graph algorithms built on top of the graph types.

- Topological sort (with cycle / non-DAG error reporting)
- Shortest-path algorithms (with negative-weight-cycle detection)
- `WeightedGraph` helpers: `getWeightsDict`, `getWeightsFilteredDict`

---

### `Prelude.SimpleTrees`
General-purpose tree structures.

- `Tree<'a>` — rose tree (`Node`, `Branch`, `Empty`)
- `SimpleBinaryTree<'a>` — binary tree with flatten and conversion utilities
- Integration with `SimpleGraphs` for graph-backed tree traversal

---

### `Prelude.Trie` / `Prelude.TrieStringSearch`
Persistent trie for prefix-keyed storage and string search.

- `trie<'k,'a>` — generic trie keyed on any comparable type
- Insert, lookup, subtrie navigation, and path enumeration
- `TrieStringSearch` — string-optimized trie for prefix/substring search

---

### `Prelude.StringMetrics`
String and sequence similarity metrics.

- `bithamming` — generic bit-level Hamming distance (u64, u32, byte variants)
- `LcsTrace` — longest common subsequence table with traceback
- LCS-based diff and reconstruction utilities

---

### `Prelude.Math.Pareto`
Multi-objective ranking and Pareto frontier utilities.

- `Pareto.findFrontier` — filter a sequence to its non-dominated Pareto front
- `Pareto.calculateRanks` — assign Pareto ranks to all points
- `Pareto.compactParetoEfficiency` — group points by rank for display/analysis
- `TextHistogram` — ASCII histogram rendering for distributions

---

### `Prelude.Control` — Hierarchical Finite State Machines
A memory-carrying flat and hierarchical FSM (HFSM) library.

Key design points:
- **Named states + explicit memory** — transitions carry both next state and updated context; no hidden mutable state
- **Hierarchical fallback dispatch** — parent states handle events unhandled by children, without a full statechart runtime
- **Multiple execution policies** — run-to-completion, bounded (`MaxTransitions`), yielding, and waiting
- **Optional actor ownership** — same machine model usable as a direct stepper or a serialized message-driven component
- **Definition/runtime split** — one compiled HFSM shared across many live instances for high-instance-count workloads

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

See [state-machine-overview.md](Prelude/state-machine-overview.md) and [control.tutorial.md](Prelude/control.tutorial.md) for full API documentation and worked examples.

---

## License

See [license.txt](Prelude/license.txt).
