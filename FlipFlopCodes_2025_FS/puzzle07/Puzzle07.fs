module Puzzle07

let findAllPathsIn2D (grid: int[,]) (startRow: int) (startCol: int) (endRow: int) (endCol: int) : (int * int) list list =
    let rows = Array2D.length1 grid
    let cols = Array2D.length2 grid
    
    // 4 directions: up, down, left, right
    let directions = [(-1, 0); (1, 0); (0, -1); (0, 1)]
    
    let isValid r c visited = 
        r >= 0 && r < rows && c >= 0 && c < cols && not (Set.contains (r, c) visited)
    
    let rec dfs r c path visited =
        if r = endRow && c = endCol then
            [path |> List.rev]  // Found a path
        else
            directions
            |> List.collect (fun (dr, dc) ->
                let nr, nc = r + dr, c + dc
                if isValid nr nc visited then
                    let newPath = (nr, nc) :: path
                    let newVisited = Set.add (nr, nc) visited
                    dfs nr nc newPath newVisited
                else
                    []
            )
    
    dfs startRow startCol [(startRow, startCol)] (Set.singleton (startRow, startCol))

let countShortestPathsIn3D (grid: int[,,]) (startX: int) (startY: int) (startZ: int) (endX: int) (endY: int) (endZ: int) : int =
    let rows = Array3D.length1 grid
    let cols = Array3D.length2 grid
    let depth = Array3D.length3 grid
    
    let directions = [
        (-1, 0, 0); (1, 0, 0); (0, -1, 0); (0, 1, 0); (0, 0, -1); (0, 0, 1)
    ]
    
    let dist = Array3D.create rows cols depth System.Int32.MaxValue
    let count = Array3D.create rows cols depth 0
    dist.[startX, startY, startZ] <- 0
    count.[startX, startY, startZ] <- 1
    let queue = System.Collections.Generic.Queue<int * int * int>()
    queue.Enqueue((startX, startY, startZ))
    
    while queue.Count > 0 do
        let (r, c, d) = queue.Dequeue()
        for (dr, dc, dd) in directions do
            let nr, nc, nd = r + dr, c + dc, d + dd
            if nr >= 0 && nr < rows && nc >= 0 && nc < cols && nd >= 0 && nd < depth then
                let newDist = dist.[r, c, d] + 1
                if newDist < dist.[nr, nc, nd] then
                    dist.[nr, nc, nd] <- newDist
                    count.[nr, nc, nd] <- count.[r, c, d]
                    queue.Enqueue((nr, nc, nd))
                elif newDist = dist.[nr, nc, nd] then
                    count.[nr, nc, nd] <- count.[nr, nc, nd] + count.[r, c, d]
    
    count.[endX, endY, endZ]


// Part 1
let SolvePart1 () =
    let grids = 
            LocalHelper.ReadFileAsLines false 7
            |> Seq.map(fun l -> int(l.Split(" ")[0]), int(l.Split(" ")[1]))
            |> List.ofSeq
    let results =
        grids
        |> List.map (fun (x, y) -> 
            findAllPathsIn2D (Array2D.create x y 0) 0 0 (x-1) (y-1)
            |> List.groupBy _.Length
            |> List.sortBy fst
            |> List.map (fun (a, b) -> b.Length)
            |> List.head
        )
    results
    |> List.sum        

// Part 2
let SolvePart2 () =
    let grids = 
        LocalHelper.ReadFileAsLines false 7
        |> Seq.map(fun l -> int(l.Split(" ")[0]), int(l.Split(" ")[1]))
        |> List.ofSeq
    let results =
        grids
        |> List.map (fun (x, y) -> 
            countShortestPathsIn3D (Array3D.create x y x 0) 0 0 0 (x-1) (y-1) (x-1)
        )
    results
    |> List.sum

// Part 3
let SolvePart3 () =
    let factorial (n: int) =
        let mutable result = bigint 1
        for i in 2..n do result <- result * bigint i
        result

    let shortestPathsND (dims: int) (sideLen: int) =
        let stepsPerDim = sideLen - 1
        let totalSteps = dims * stepsPerDim
        factorial totalSteps / (pown (factorial stepsPerDim) dims)

    let grids =
        LocalHelper.ReadFileAsLines false 7
        |> Seq.map (fun l -> int(l.Split(" ")[0]), int(l.Split(" ")[1]))
    
    grids
    |> Seq.sumBy (fun (dims, sideLen) -> shortestPathsND dims sideLen)