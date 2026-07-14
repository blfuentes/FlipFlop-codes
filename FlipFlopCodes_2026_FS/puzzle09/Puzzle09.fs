module Puzzle09

open LocalHelper
open System.Collections.Generic

let isTest = true
let mutable (maxRows, maxCols) = (0, 0)

type CellType =
| Wall
| Empty

type Cell = {
    Row: int
    Col: int
    Kind: CellType
}

// Part 1
let SolvePart1 =
    let input = ReadFileAsLines isTest 9
    maxRows <- input.Length
    maxCols <- input[0].Length
    let mutable (startRow, startCol) = (0, 0)
    let mutable (endRow, endCol) = (0, 0)
    let maze = Array2D.init<Cell> maxRows maxCols (fun r c ->
        let t =
            match input[r][c] with
            | '#' ->
                Wall
            | 'S' ->
                startRow <- r
                startCol <- c
                Empty
            | '.' ->
                Empty
            | 'E' ->
                endRow <- r
                endCol <- c
                Empty
            | _ -> failwith "invalid cell"
        { Row = r; Col = c; Kind = t }
    )

    let getNeighbors (maze: Cell[,]) (x, y) =
        let rows = maze.GetLength(0)
        let cols = maze.GetLength(1)
        [ (x-1, y); (x+1, y); (x, y-1); (x, y+1) ]
        |> List.filter (fun (r, c) ->
            r >= 0 && r < rows && c >= 0 && c < cols &&
            match maze[r, c].Kind with Wall -> false | _ -> true
        )

    let visited = HashSet<int*int>()
    let parent = Dictionary<Cell, Cell>()
    let queue = Queue<Cell>()
    let start = maze[startRow, startCol]
    let target = maze[endRow, endCol]
    queue.Enqueue(start)
    let mutable found = false
    while queue.Count > 0 && not found do
        let current = queue.Dequeue()
        if current = target then
            found <- true
        else
            for n in getNeighbors maze (current.Row, current.Col) do
                if not (visited.Contains n) then
                    visited.Add(n) |> ignore
                    let parentPos = maze[fst n, snd n]
                    parent[parentPos] <- current
                    queue.Enqueue(parentPos)
    let rec buildPath acc pos =
        if pos = start then start :: acc
        elif parent.ContainsKey(pos) then buildPath (pos :: acc) parent[pos]
        else []

    let path = if found then buildPath [] target else []
    path.Length - 1 // remove starting point      

// Part 2
let SolvePart2 =
    let input = ReadFileAsLines isTest 9
    maxRows <- input.Length
    maxCols <- input[0].Length
    let mutable (startRow, startCol) = (0, 0)
    let mutable (endRow, endCol) = (0, 0)
    let maze = Array2D.init<Cell> maxRows maxCols (fun r c ->
        let t =
            match input[r][c] with
            | '#' ->
                Wall
            | 'S' ->
                startRow <- r
                startCol <- c
                Empty
            | '.' ->
                Empty
            | 'E' ->
                endRow <- r
                endCol <- c
                Empty
            | _ -> failwith "invalid cell"
        { Row = r; Col = c; Kind = t }
    )

    let getNeighbors (maze: Cell[,]) (x, y) =
        let rows = maze.GetLength(0)
        let cols = maze.GetLength(1)
        [ (x-1, y); (x+1, y); (x, y-1); (x, y+1) ]
        |> List.filter (fun (r, c) ->
            r >= 0 && r < rows && c >= 0 && c < cols &&
            match maze[r, c].Kind with Wall -> false | _ -> true
        )

    let visited = HashSet<int*int>()
    let parent = Dictionary<Cell, Cell>()
    let queue = Queue<Cell>()
    let start = maze[startRow, startCol]
    let target = maze[endRow, endCol]
    queue.Enqueue(start)
    let mutable found = false
    while queue.Count > 0 && not found do
        let current = queue.Dequeue()
        if current = target then
            found <- true
        else
            for n in getNeighbors maze (current.Row, current.Col) do
                if not (visited.Contains n) then
                    visited.Add(n) |> ignore
                    let parentPos = maze[fst n, snd n]
                    parent[parentPos] <- current
                    queue.Enqueue(parentPos)
    let rec buildPath acc pos =
        if pos = start then start :: acc
        elif parent.ContainsKey(pos) then buildPath (pos :: acc) parent[pos]
        else []

    let path = (if found then buildPath [] target else []) |> Array.ofList

    let mutable direction = if path[0].Row = path[1].Row then 'h' else 'v'
    let mutable steps = 1
    let mutable acc = 0
    for (from, target) in (path[1..] |> Seq.pairwise) do
        let newdirection = if from.Row = target.Row then 'h' else 'v'
        let nextExpected =
            match (newdirection, from.Col - target.Col, from.Row - target.Row) with
            | ('h', diff, _) when diff < 0 ->
                // horizontally to the right
                maze[from.Row, target.Col + 1]
            | ('h', diff, _) when diff > 0 ->
                // horizontally to the left, keep up and down
                maze[from.Row, target.Col - 1]
            | ('v', _, diff) when diff < 0 ->
                // vertically to the bottom, keep left and right
                maze[target.Row + 1, from.Col]
            | ('v', _, diff) when diff > 0 ->
                // vertically to the top, keep left and right
                maze[target.Row - 1, from.Col]

        acc <- acc + 1
        if direction <> newdirection then 
            steps <- steps + (if nextExpected.Kind.IsWall then 1 else acc)
            acc <- 0
            
        direction <- newdirection

    steps

// Part 3
let SolvePart3 =
    0