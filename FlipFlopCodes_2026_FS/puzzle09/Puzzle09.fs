module Puzzle09

open LocalHelper
open System.Collections.Generic

let isTest = false
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
let SolvePart1 () =
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
let SolvePart2 () =
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

    let directions = [ (-1, 0); (1, 0); (0, -1); (0, 1) ]

    let isOpen (row, col) =
        row >= 0 && row < maxRows && col >= 0 && col < maxCols &&
        maze[row, col].Kind <> Wall

    let getPortalDestination (row, col) (rowDelta, colDelta) =
        let mutable destination = (row, col)
        let mutable next = (row + rowDelta, col + colDelta)

        while isOpen next do
            destination <- next
            next <- (fst next + rowDelta, snd next + colDelta)

        destination

    let getPortalNeighbors position =
        directions
        |> List.collect (fun (rowDelta, colDelta) ->
            let walk = (fst position + rowDelta, snd position + colDelta)
            let portal = getPortalDestination position (rowDelta, colDelta)

            [ if isOpen walk then walk
              if portal <> position then portal ])
        |> List.distinct

    let visited = HashSet<int*int>()
    let queue = Queue<(int * int) * int>()
    let start = (startRow, startCol)
    let target = (endRow, endCol)
    visited.Add(start) |> ignore
    queue.Enqueue((start, 0))

    let mutable shortestPath = None
    while queue.Count > 0 && shortestPath.IsNone do
        let current, steps = queue.Dequeue()

        if current = target then
            shortestPath <- Some steps
        else
            for neighbor in getPortalNeighbors current do
                if visited.Add(neighbor) then
                    queue.Enqueue((neighbor, steps + 1))

    shortestPath |> Option.defaultValue -1

// Part 3
let SolvePart3 () =
    let input = ReadFileAsLines isTest 9
    let rows = input.Length
    let cols = input[0].Length
    let mutable start = (0, 0)
    let mutable target = (0, 0)

    for row in 0 .. rows - 1 do
        for col in 0 .. cols - 1 do
            match input[row][col] with
            | 'S' -> start <- (row, col)
            | 'E' -> target <- (row, col)
            | _ -> ()

    let rowDeltas = [| -1; 1; 0; 0 |]
    let colDeltas = [| 0; 0; -1; 1 |]
    let cellCount = rows * cols

    let openCells = Array.zeroCreate<bool> cellCount
    for row in 0 .. rows - 1 do
        for col in 0 .. cols - 1 do
            openCells[row * cols + col] <- input[row][col] <> '#'

    let moveTargets = Array.create (cellCount * 4) -1
    let teleportTargets = Array.create (cellCount * 4) -1
    let hasPortalEntrance = Array.zeroCreate<bool> cellCount

    for position in 0 .. cellCount - 1 do
        if openCells[position] then
            let row = position / cols
            let col = position % cols

            for direction in 0 .. 3 do
                let rowDelta = rowDeltas[direction]
                let colDelta = colDeltas[direction]
                let mutable portalRow = row
                let mutable portalCol = col
                let mutable nextRow = row + rowDelta
                let mutable nextCol = col + colDelta

                if nextRow >= 0 && nextRow < rows && nextCol >= 0 && nextCol < cols then
                    if openCells[nextRow * cols + nextCol] then
                        moveTargets[position * 4 + direction] <- nextRow * cols + nextCol
                    else
                        hasPortalEntrance[position] <- true

                while nextRow >= 0 && nextRow < rows && nextCol >= 0 && nextCol < cols &&
                      openCells[nextRow * cols + nextCol] do
                    portalRow <- nextRow
                    portalCol <- nextCol
                    nextRow <- nextRow + rowDelta
                    nextCol <- nextCol + colDelta

                if nextRow >= 0 && nextRow < rows && nextCol >= 0 && nextCol < cols &&
                   input[nextRow][nextCol] = '#' then
                    let teleportTarget = portalRow * cols + portalCol
                    if teleportTarget <> position then
                        teleportTargets[position * 4 + direction] <- teleportTarget

    let startPosition = fst start * cols + snd start
    let targetPosition = fst target * cols + snd target
    let startState = startPosition * 2
    let distances = Array.create (cellCount * 2) System.Int32.MaxValue
    let queue = PriorityQueue<struct (int * int), int>()
    distances[startState] <- 0
    queue.Enqueue(struct (startState, 0), 0)

    let timer = System.Diagnostics.Stopwatch.StartNew()
    let mutable processedStates = 0L
    let mutable shortestPath = -1

    while queue.Count > 0 && shortestPath < 0 do
        let struct (state, distance) = queue.Dequeue()

        if distance = distances[state] then
            let position = state / 2
            let hasJustTeleported = (state &&& 1) <> 0
            processedStates <- processedStates + 1L

            if position = targetPosition then
                shortestPath <- distance
            else
                for direction in 0 .. 3 do
                    let moveTarget = moveTargets[position * 4 + direction]
                    if moveTarget >= 0 then
                        let nextState = moveTarget * 2
                        let nextDistance = distance + 1
                        if nextDistance < distances[nextState] then
                            distances[nextState] <- nextDistance
                            queue.Enqueue(struct (nextState, nextDistance), nextDistance)

                if hasPortalEntrance[position] then
                    let teleportCost = if hasJustTeleported then 2 else 3

                    for direction in 0 .. 3 do
                        let teleportTarget = teleportTargets[position * 4 + direction]
                        if teleportTarget >= 0 then
                            let nextState = teleportTarget * 2 + 1
                            let nextDistance = distance + teleportCost
                            if nextDistance < distances[nextState] then
                                distances[nextState] <- nextDistance
                                queue.Enqueue(struct (nextState, nextDistance), nextDistance)

    //printfn "Finishedinished: result=%d, processed=%d, states=%d, elapsed=%O"
    //    shortestPath processedStates distances.Length timer.Elapsed

    shortestPath