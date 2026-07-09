module Puzzle04

open System.Collections.Generic

// Part 1
let SolvePart1 =
    let plant = LocalHelper.ReadFileAsLines false 4
    let leftSide = plant |> Seq.map _.Substring(0, 1) |> Seq.toArray
    let rightSide = plant |> Seq.map _.PadRight(5).Substring(4,1) |> Seq.toArray
    let cutLevel = 400
    let leaves =
        (leftSide[..leftSide.Length-1 - cutLevel - 1] |> Array.filter(fun p -> p = "o") |> Array.length) +
        (rightSide[..rightSide.Length-1 - cutLevel - 1] |> Array.filter(fun p -> p = "o") |> Array.length)
    leaves  

// Part 2
let SolvePart2 =
    let plant = LocalHelper.ReadFileAsLines false 4
    let leftSide = plant |> Seq.map _.Substring(0, 1) |> Seq.toArray
    let rightSide = plant |> Seq.map _.PadRight(5).Substring(4,1) |> Seq.toArray
    let mutable jumpIdx = plant.Length - 2
    let mutable side = if leftSide[jumpIdx] = "o" then 0 else 1
    let mutable jumps = 0
    while jumpIdx > 2 do
        match (side, leftSide[jumpIdx-1], rightSide[jumpIdx-1]) with
        | (s, l, r) when s = 0 && r = "o" -> 
            jumps <- jumps + 1
            side <- 1
        | (s, l, r) when s = 1 && l = "o" ->
            jumps <- jumps + 1
            side <- 0
        | _ -> ignore()
        jumpIdx <- jumpIdx - 1
    jumps

// Part 3
let SolvePart3 =
    let plant = LocalHelper.ReadFileAsLines true 4
    let leftSide = 
        plant 
        |> Seq.map _.Substring(0, 1) 
        |> Seq.toArray
    let rightSide = 
        plant 
        |> Seq.map _.PadRight(5).Substring(4,1) 
        |> Seq.toArray
    let leftIdxes = Stack<int>()
    let rightIdxes = Stack<int>()
    leftSide
        |> Array.indexed 
        |> Array.iter(fun (i, v) -> if v = "o" then leftIdxes.Push(i) else ignore())
    rightSide 
        |> Array.indexed 
        |> Array.iter(fun (i, v) -> if v = "o" then rightIdxes.Push(i) else ignore())

    let mutable jumpIdx = plant.Length - 2
    let mutable side = if leftSide[jumpIdx] = "o" then 0 else 1
    let mutable workers = 0
    while(leftIdxes.Count > 0 || rightIdxes.Count > 0) do
        workers <- workers + 1
        let (lIdx, rIdx) = (
            (if leftIdxes.Count > 0 then leftIdxes.Peek() else -1), 
            (if rightIdxes.Count > 0 then rightIdxes.Peek() else -1)
            )
        if lIdx > rIdx then 
            side <- 0
            jumpIdx <- leftIdxes.Pop()
        else
            side <- 1
            jumpIdx <- rightIdxes.Pop()
        while jumpIdx > 2 do
            match (side, leftSide[jumpIdx-1], rightSide[jumpIdx-1]) with
            | (s, l, r) when s = 0 && r = "o" ->                
                side <- 1
                jumpIdx <- if rightIdxes.Count > 0 then rightIdxes.Pop() else 2
            | (s, l, r) when s = 1 && l = "o" ->
                side <- 0
                jumpIdx <- if leftIdxes.Count > 0 then leftIdxes.Pop() else 2
            | _ ->
                jumpIdx <- 
                    if side = 0 then 
                        if leftIdxes.Count > 0 then leftIdxes.Peek() else 2
                    else 
                        if rightIdxes.Count > 0 then rightIdxes.Peek() else 2
    workers