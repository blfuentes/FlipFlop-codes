module Puzzle05

open System.Collections.Generic

let printMap (grid: char[,]) =
    for r in 0..grid.GetLength(0) - 1 do
        for c in 0..grid.GetLength(1) - 1 do
           printf "%c" (grid[r, c])
        printfn ""

let printVisitedMap (visited: HashSet<int*int>) (grid: char[,]) =
    for r in 0..grid.GetLength(0) - 1 do
        for c in 0..grid.GetLength(1) - 1 do
           printf "%c" (if visited.Contains (r, c) then (grid[r, c]) else '.')
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
    let visited = HashSet<int*int>()
    
    let rec countNonVisited toCheck =
        if visited.Contains toCheck then
            visited.Count
        else
            visited.Add toCheck |> ignore
            let newPos = nextPos (map[fst toCheck, snd toCheck]) toCheck
            countNonVisited newPos
    countNonVisited (0, 0)

// Part 2
let SolvePart2 =
    let content = LocalHelper.ReadFileAsLines false 5
    let maxRows = content.Length
    let maxCols = content[0].Length
    let map = Array2D.init maxRows maxCols (fun r c ->content[r][c])

    let rec countNonVisited (visited: HashSet<int*int>) toCheck toReplace replaceMov =
        if visited.Contains toCheck then
            visited.Count
        else
            visited.Add toCheck |> ignore
            let mov = if toCheck = toReplace then replaceMov else map[fst toCheck, snd toCheck]
            let newPos = nextPos mov toCheck
            countNonVisited visited newPos toReplace replaceMov

    [
        for pR in 1..(map.GetLength(0) - 2) do
            for pC in 1..(map.GetLength(1) - 2) do
                for mov in ['v'; '^'; '>'; '<'] do
                    yield countNonVisited (new HashSet<int*int>()) (0, 0) (pR, pC) mov
    ] |> Seq.max

// Part 3
let SolvePart3 =
    let content = LocalHelper.ReadFileAsLines false 5
    let maxRows = content.Length
    let maxCols = content[0].Length
    let map = Array2D.init maxRows maxCols (fun r c ->content[r][c])
    let isBorder (row, col) =
        row = 0 || row = maxRows-1 || col = 0 || col = maxCols-1

    let rec countNonVisited (theMap: char[,]) (visited: HashSet<int*int>) toCheck consumedHacks =
        if visited.Contains toCheck then
            if consumedHacks = 3 || isBorder toCheck then                
                visited.Count
            else
                let mov = 
                    match theMap[fst toCheck, snd toCheck] with
                    | '^' -> '>'
                    | '>' -> 'v'
                    | 'v' -> '<'
                    | '<' -> '^'
                    | _ -> failwith "invalid turn"
                let newPos = nextPos mov toCheck
                countNonVisited theMap visited newPos (consumedHacks + 1)
        else
            visited.Add toCheck |> ignore
            let newPos = nextPos theMap[fst toCheck, snd toCheck] toCheck
            countNonVisited theMap visited newPos consumedHacks

    [
        for pR in 1..(map.GetLength(0) - 2) do
            for pC in 1..(map.GetLength(1) - 2) do
                for mov in ['v'; '^'; '>'; '<'] do
                    let replacementMap = Array2D.copy map
                    replacementMap[pR, pC] <- mov
                    yield countNonVisited replacementMap (new HashSet<int*int>()) (0, 0) 0
    ] |> Seq.max