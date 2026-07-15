module Puzzle04

let parseContent (lines: string seq) =
    lines
    |> Seq.map(fun line ->
        (int(line.Split(",")[0]), int(line.Split(",")[1]))
    )

let manhattanDistance ((x1, y1), (x2, y2)) =
    abs (x1 - x2) + abs (y1 - y2)

let chebyshevDistance ((x1, y1), (x2, y2)) =
    max (abs (x1 - x2)) (abs (y1 - y2))

// Part 1
let SolvePart1 () =
    let trashes = parseContent (LocalHelper.ReadFileAsLines false 4)
    // concat seq 'trashes' with array [|0;0|]
    Seq.append [(0,0)] trashes
    |> Seq.pairwise
    |> Seq.sumBy manhattanDistance

// Part 2
let SolvePart2 () =
    let trashes = parseContent (LocalHelper.ReadFileAsLines false 4)
    // concat seq 'trashes' with array [|0;0|]
    Seq.append [(0,0)] trashes
    |> Seq.pairwise
    |> Seq.sumBy chebyshevDistance

// Part 3
let SolvePart3 () =
    let trashes = parseContent (LocalHelper.ReadFileAsLines false 4)
    // concat seq 'trashes' with array [|0;0|]
    Seq.append [(0,0)] trashes
    |> Seq.sortBy (fun (x, y) -> manhattanDistance ((0, 0), (x, y)))
    |> Seq.pairwise
    |> Seq.sumBy chebyshevDistance