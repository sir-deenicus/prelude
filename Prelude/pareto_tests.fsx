#r @"bin/Debug/net5/Prelude.dll"

open System
open Prelude.Math

let assertEqual expected actual =
    if actual <> expected then
        failwithf "Expected %A but got %A" expected actual

let isDominatedBy (ax, ay) (bx, by) =
    bx <= ax && by <= ay && (bx < ax || by < ay)

let points =
    [ (1, 4); (2, 2); (4, 1); (3, 3); (5, 5); (2, 2) ]

Pareto.findFrontier isDominatedBy points
|> assertEqual [ (1, 4); (2, 2); (4, 1); (2, 2) ]

let ranks = Pareto.calculateRanks isDominatedBy points

ranks
|> assertEqual
    [ ((1, 4), 1)
      ((2, 2), 1)
      ((4, 1), 1)
      ((2, 2), 1)
      ((3, 3), 2)
      ((5, 5), 3) ]

Pareto.compactParetoEfficiency ranks
|> assertEqual
    [ (1, [ (1, 4); (2, 2); (4, 1); (2, 2) ])
      (2, [ (3, 3) ])
      (3, [ (5, 5) ]) ]

Pareto.calculateRanks isDominatedBy []
|> assertEqual []

Pareto.calculateRanks isDominatedBy [ (1, 1) ]
|> assertEqual [ ((1, 1), 1) ]

let mutable enumerations = 0

let generatedPoints =
    seq {
        enumerations <- enumerations + 1
        yield! points
    }

Pareto.findFrontier isDominatedBy generatedPoints |> ignore
assertEqual 1 enumerations

[<CustomEquality; NoComparison>]
type CoarseEqualityPoint =
    { Id: int
      Cost: int
      Risk: int }

    override this.Equals other =
        match other with
        | :? CoarseEqualityPoint as point -> this.Id = point.Id
        | _ -> false

    override this.GetHashCode() = hash this.Id

let coarseEqualityPoints =
    [ { Id = 1; Cost = 1; Risk = 1 }
      { Id = 1; Cost = 2; Risk = 2 } ]

let isCoarsePointDominated candidate other =
    other.Cost <= candidate.Cost
    && other.Risk <= candidate.Risk
    && (other.Cost < candidate.Cost || other.Risk < candidate.Risk)

Pareto.calculateRanks isCoarsePointDominated coarseEqualityPoints
|> List.map (fun (point, rank) -> (point.Cost, point.Risk), rank)
|> assertEqual [ ((1, 1), 1); ((2, 2), 2) ]

let cyclic candidate other =
    match candidate, other with
    | 1, 2 | 2, 3 | 3, 1 -> true
    | _ -> false

let assertInvalidDominance relation points =
    try
        Pareto.calculateRanks relation points |> ignore
        failwith "Expected the invalid dominance relation to be rejected."
    with :? ArgumentException ->
        ()

assertInvalidDominance cyclic [ 1; 2; 3 ]
assertInvalidDominance (fun candidate other -> other <= candidate) [ 1; 2; 3 ]

printfn "Pareto tests passed."
