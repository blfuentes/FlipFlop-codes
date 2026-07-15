module Puzzle04

open System.Collections.Generic

// Part 1
let SolvePart1 () =
    let plant = LocalHelper.ReadFileAsLines false 4
    let leftSide = plant |> Seq.map _.Substring(0, 1) |> Seq.toArray
    let rightSide = plant |> Seq.map _.PadRight(5).Substring(4,1) |> Seq.toArray
    let cutLevel = 400
    (leftSide[..leftSide.Length - 2 - cutLevel] |> Array.filter(fun p -> p = "o") |> Array.length) +
    (rightSide[..rightSide.Length - 2 - cutLevel] |> Array.filter(fun p -> p = "o") |> Array.length)

// Part 2
let SolvePart2 () =
    let plant = 
        [for line in LocalHelper.ReadFileAsLines false 4 |> Seq.skip 2 do
            if line.StartsWith("o-|") then
                yield -1
            elif line.EndsWith("|-o") then
                yield 1
        ]
    plant
    |> Seq.pairwise
    |> Seq.filter(fun (a, b) -> a <> b)
    |> Seq.length   

// Part 3
let SolvePart3 () =
    let plant = LocalHelper.ReadFileAsLines false 4
    let leftSide = 
        plant 
        |> Seq.map _.Substring(0, 1) 
        |> Seq.toArray
    let rightSide = 
        plant 
        |> Seq.map _.PadRight(5).Substring(4,1) 
        |> Seq.toArray
    
    let consumed = HashSet<int>()
    let mutable workers = 0
    let mutable (side, initJumpIdx) = (0, leftSide.Length - 2)
    while initJumpIdx > 2 do
        let mutable doJump = true
        let firstLeft = leftSide |> Array.tryFindIndexBack(fun l -> l = "o")
        let firstRight = rightSide |> Array.tryFindIndexBack(fun r -> r = "o")
        let (s, i) =
            match (firstLeft, firstRight) with
            | (Some(a), Some(b)) when a > b -> (0, a)
            | (Some(a), Some(b)) when a < b -> (1, b)
            | (Some(a), None) -> (0, a)
            | (None, Some(b)) -> (1, b)
            | _ -> 
                doJump <- false
                (0, 0)
        side <- s
        initJumpIdx <- i

        let mutable innerJumpIdx = initJumpIdx
        let mutable lastJumpIdx = innerJumpIdx

        while innerJumpIdx > 2 do
            match (side, leftSide[innerJumpIdx - 1], rightSide[innerJumpIdx - 1]) with
            | (s, l, r) when s = 0 && r = "o" ->
                side <- 1 // jump to other side
                leftSide[lastJumpIdx] <- "" // remove leaf
                lastJumpIdx <- innerJumpIdx - 1
            | (s, l, r) when s = 1 && l = "o" ->
                side <- 0 // jump to other side
                rightSide[lastJumpIdx] <- "" // remove leaf
                lastJumpIdx <- innerJumpIdx - 1
            | (s, l, r) when l = "o" || r = "o" ->
                lastJumpIdx <- innerJumpIdx - 1 // stay in the same side jumping up
            | _ -> ignore()
            innerJumpIdx <- innerJumpIdx - 1
        if side = 0 then leftSide[lastJumpIdx] <- "" else rightSide[lastJumpIdx] <- "" // remove last used leaf
        if doJump then workers <- workers + 1 // worker jumped
    workers    
