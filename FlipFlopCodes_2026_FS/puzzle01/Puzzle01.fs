module Puzzle01

open LocalHelper

// Part 1
let SolvePart1 =
    LocalHelper.ReadFileAsLines false 1 
    |> Seq.sumBy(fun t' ->
        let t = int(t')
        if t < 60 then 60-t else 0
    )
    

// Part 2
let SolvePart2 =
    LocalHelper.ReadFileAsLines false 1 
    |> Seq.sumBy(fun t' ->
        let t = int(t')
        abs(60-t) * (if t < 60 then 1 else 5)
    )

// Part 3
let SolvePart3 =
    let input = LocalHelper.ReadFileAsLines false 1
    let (temperatures, desired) = (input[.. input.Length / 2 - 1], input[input.Length / 2..])
    temperatures
    |> Seq.mapi(fun idx t' -> 
        let t = int(t')
        let d = int(desired[idx])
        abs(d-t) * (if t < d then 1 else 5)
    )
    |> Seq.sum