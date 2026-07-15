module Puzzle07

let isTest = false
let (maxGridX, maxGridY) = if isTest then (10, 10) else (30, 30)
let toConsume = if isTest then 20 else 2500
let (startX, startY) = (0, 0)

// Part 1
let SolvePart1 () =
    let input = LocalHelper.ReadFileAsLines isTest 7
    let instructions = input[0]
    let sushis = input[2..] |> Seq.map (fun l -> (int(l.Split(',')[0]), int(l.Split(',')[1]))) |> Array.ofSeq
    let ((finishX, finishY), consumed) = 
        ([0..(toConsume-1)])
        |> Seq.fold(fun ((x, y), sushiIdx) c ->
            let (nX, nY) =
                match instructions[c] with
                | '^' -> (x, y + 1)
                | '>' -> (x + 1, y)
                | 'v' -> (x, y - 1)
                | '<' -> (x - 1, y)
                | _ -> failwith "invalid instruction"
            ((nX, nY), if sushis[sushiIdx] = (nX, nY) then sushiIdx + 1 else sushiIdx)
        ) ((startX, startY), 0)
    consumed

// Part 2
let SolvePart2 () =
    let input = LocalHelper.ReadFileAsLines isTest 7
    let instructions = input[0]
    let sushis = input[2..] |> Seq.map (fun l -> (int(l.Split(',')[0]), int(l.Split(',')[1]))) |> Array.ofSeq
    let rec eatTilDeath (snake: (int*int) list) ((x, y): int*int) (sushiIdx: int) (insIdx: int) =
        let (nX, nY) =
            match instructions[insIdx] with
            | '^' -> (x, y + 1)
            | '>' -> (x + 1, y)
            | 'v' -> (x, y - 1)
            | '<' -> (x - 1, y)
            | _ -> failwith "invalid instruction"
        let iEatMySelf = 
            match snake |> List.tryFindIndex ((=) (nX, nY)) with
            | Some i when i = snake.Length - 1 -> false
            | Some i -> true
            | None -> false
        if iEatMySelf then
            snake.Length
        else
            let (nSushiIdx, newSnake) = 
                if sushis[sushiIdx] = (nX, nY) then
                    ((sushiIdx + 1), [(nX, nY)] @ snake)
                else
                    let subSnake = snake |> List.removeAt(snake.Length-1)
                    (sushiIdx, [(nX, nY)] @ subSnake)

            eatTilDeath newSnake (nX, nY) nSushiIdx (insIdx + 1)
    eatTilDeath [(startX, startY)] (startX, startY) 0 0

// Part 3
let SolvePart3 () =
    let input = LocalHelper.ReadFileAsLines isTest 7
    let instructions = input[0]
    let sushis = input[2..] |> Seq.map (fun l -> (int(l.Split(',')[0]), int(l.Split(',')[1]))) |> Array.ofSeq
    let rec eatTilDeath (snake: (int*int) list) ((x, y): int*int) (sushiIdx: int) (insIdx: int) eats =
        if insIdx = instructions.Length then 
            snake.Length * eats
        else
            let (nX, nY) =
                match instructions[insIdx] with
                | '^' -> (x, y + 1)
                | '>' -> (x + 1, y)
                | 'v' -> (x, y - 1)
                | '<' -> (x - 1, y)
                | _ -> failwith "invalid instruction"
            let (iEatMySelf, idx, newEats) = 
                match snake |> List.tryFindIndex ((=) (nX, nY)) with
                | Some i when i = snake.Length - 1 -> (false, 0, eats)
                | Some i -> (true, i, eats + 1)
                | None -> (false, 0, eats)

            let snakeParts = if iEatMySelf then snake |> List.take idx else snake

            let (nSushiIdx, newSnake) = 
                if sushiIdx < sushis.Length && sushis[sushiIdx] = (nX, nY) then
                    ((sushiIdx + 1), [(nX, nY)] @ snakeParts)
                else
                    let subSnake = snakeParts |> List.removeAt(snakeParts.Length-1)
                    (sushiIdx, [(nX, nY)] @ subSnake)
        
            eatTilDeath newSnake (nX, nY) nSushiIdx (insIdx + 1) newEats
    eatTilDeath [(startX, startY)] (startX, startY) 0 0 0