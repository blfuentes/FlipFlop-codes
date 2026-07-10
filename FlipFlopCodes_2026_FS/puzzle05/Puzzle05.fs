module Puzzle05

open System.Collections.Generic

let printMap (grid: char[,]) =
    for r in 0..grid.GetLength(0) - 1 do
        for c in 0..grid.GetLength(1) - 1 do
           printf "%c" (grid[r, c])
        printfn ""
    
let nextPos mov (fromRow, fromCol) =
    match mov with
    | '>' -> (fromRow, fromCol + 1)
    | '<' -> (fromRow, fromCol - 1)
    | 'v' -> (fromRow + 1, fromCol)
    | '^' -> (fromRow - 1, fromCol)
    | _ -> failwith "invalid movement"

// Part 1
let SolvePart1 =
    let content = LocalHelper.ReadFileAsLines false 5
    let maxRows = content.Length
    let maxCols = content[0].Length
    let map = Array2D.init maxRows maxCols (fun r c ->content[r][c])
    //printMap map
    let visited = HashSet<int*int>()
    
    let rec countNonVisited toCheck count =
        if visited.Contains toCheck then
            count
        else
            visited.Add toCheck |> ignore
            let newPos = nextPos (map[fst toCheck, snd toCheck]) toCheck
            //printf "%c" map[fst toCheck, snd toCheck]
            countNonVisited newPos count+1
    countNonVisited (0, 0) 0

// Part 2
let SolvePart2 =
    let content = LocalHelper.ReadFileAsLines false 5
    let maxRows = content.Length
    let maxCols = content[0].Length
    let map = Array2D.init maxRows maxCols (fun r c ->content[r][c])
    let movements = ['v'; '^'; '>'; '<']

    let rec countNonVisited (visited: HashSet<int*int>) toCheck toReplace replaceMov count =
        if visited.Contains toCheck then
            count
        else
            visited.Add toCheck |> ignore
            let mov = if toCheck = toReplace then replaceMov else map[fst toCheck, snd toCheck]
            let newPos = nextPos mov toCheck
            //printf "%c" map[fst toCheck, snd toCheck]
            countNonVisited visited newPos toReplace replaceMov count+1

    [
        for pR in 1..(map.GetLength(0) - 2) do
            for pC in 1..(map.GetLength(1) - 2) do
                for mov in movements do
                    yield countNonVisited (new HashSet<int*int>()) (0, 0) (pR, pC) mov 0
    ] |> Seq.max

// Part 3
let SolvePart3 =
    let content = LocalHelper.ReadFileAsLines true 5
    let maxRows = content.Length
    let maxCols = content[0].Length
    let map = Array2D.init maxRows maxCols (fun r c ->content[r][c])
    //printMap map
    let isBorder (row, col) =
        row = 0 || row = maxRows-1 || col = 0 || col = maxCols-1

    let rec countNonVisited (visited: HashSet<int*int>) toCheck toReplace replaceMov count consumedHacks =
        if visited.Contains toCheck then
            if consumedHacks = 3 || isBorder toCheck then
                printfn "replacement (%d, %d) %c finished with %d" (fst toReplace)(snd toReplace) replaceMov count
                count
            else
                let mov = 
                    match map[fst toCheck, snd toCheck] with
                    | '^' -> '>'
                    | '>' -> 'v'
                    | 'v' -> '<'
                    | '<' -> '^'
                    | _ -> failwith "invalid hack"
                let newPos = nextPos mov toCheck
                countNonVisited visited newPos toReplace replaceMov count (consumedHacks + 1)
        else
            visited.Add toCheck |> ignore
            let mov = if toCheck = toReplace then replaceMov else map[fst toCheck, snd toCheck]
            let newPos = nextPos mov toCheck
            //printf "%c" map[fst toCheck, snd toCheck]
            countNonVisited visited newPos toReplace replaceMov (count + 1) consumedHacks
    [
        for pR in 1..(map.GetLength(0) - 2) do
            for pC in 1..(map.GetLength(1) - 2) do
                for mov in ['v'; '^'; '>'; '<'] do
                    yield countNonVisited (new HashSet<int*int>()) (0, 0) (pR, pC) mov 0 0
    ] |> Seq.max