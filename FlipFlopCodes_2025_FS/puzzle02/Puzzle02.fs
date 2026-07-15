module Puzzle02

open System.Collections
open LocalHelper
open System.Collections.Generic

// Part 1
let SolvePart1 () =
    let content = ReadFileAsText false 2
    snd (content
    |> Seq.fold(fun (current, max') c ->
        let newLevel =
            if c = '^' then current + 1
            else if c = 'v' then current - 1
            else current
        (newLevel, max newLevel max')
    ) (0, 0))

// Part 2
let SolvePart2 () =
    let content = ReadFileAsText false 2
    let (_, _, _, maxlevel) = 
        content
        |> Seq.fold(fun (prev, acc, current, max') c ->
            let newAcc = if c <> prev then 1 else acc + 1
            let newLevel =
                if c = '^' then current + newAcc
                else if c = 'v' then current - newAcc
                else current
            (c, newAcc, newLevel, max newLevel max')
        ) ('0', 0, 0, 0)
    maxlevel

// Part 3
let fibDict = new Dictionary<int, int>()

let rec fib n =
    let mutable v = 0
    if fibDict.TryGetValue(n, &v) then
        v
    else
        let result =
            match n with
            | v when v = 0 -> 0
            | v when v = 1 -> 1
            | v when v > 1 -> fib (v - 1) + fib (v - 2)
            | _ -> failwith "not valid!"
        fibDict[n] <- result
        result

let SolvePart3 () =
    let content = ReadFileAsText false 2
    let (_, _, _, max') =
        content
        |> Seq.fold(fun (prev, acc, current, max') c ->
            if prev = '-' then
                (c, 1, 0, 0)
            else
                if prev <> c then
                    let newAcc = 1
                    let newLevel =
                        if prev = '^' then current + fib acc
                        else current - fib acc
                    (c, newAcc, newLevel, max newLevel max')
                else
                    (c, acc + 1, current, max')
        ) ('-', 0, 0, 0)
    max'