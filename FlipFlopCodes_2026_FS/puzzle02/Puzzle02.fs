module Puzzle02

open System.Collections
open LocalHelper
open System.Collections.Generic

// Part 1
let SolvePart1 =
    let wallSegments = ReadFileAsText false 2
    let temperatures = Array.zeroCreate<int> 100
    wallSegments
    |> Seq.fold(fun rIdx s->
        let rIdx' =
            match s with
            | '>' -> (rIdx + 1) % 100
            | '<' -> ((rIdx - 1) + 100) % 100
            | _ -> failwith "invalid movement"
        temperatures[rIdx'] <- temperatures[rIdx'] + 1
        rIdx'
    ) 0 |> ignore
    let (pos, v) =
        temperatures
        |> Array.indexed
        |> Array.maxBy snd
    (pos + 1) * v

// Part 2
let SolvePart2 =
    let wallSegmentsForLaser = (ReadFileAsText false 2).ToCharArray()
    let wallSegmentsForRobot = wallSegmentsForLaser |> Array.rev
    let (_, _, count) =
        Seq.fold2(fun (lIdx, rIdx, count) ls rs->
            let lIdx' =
                match ls with
                | '>' -> (lIdx + 1) % 100
                | '<' -> ((lIdx - 1) + 100) % 100
                | _ -> failwith "invalid movement"
            let rIdx' =
                match rs with
                | '>' -> (rIdx + 1) % 100
                | '<' -> ((rIdx - 1) + 100) % 100
                | _ -> failwith "invalid movement"
            (lIdx', rIdx', if lIdx' = rIdx' then count + 1 else count)
        ) (0, 0, 0) wallSegmentsForLaser wallSegmentsForRobot
    count
// Part 3
let SolvePart3 =
    let wallSegmentsForLaser = (ReadFileAsText false 2).ToCharArray()
    let wallSegmentsForRobot = wallSegmentsForLaser |> Array.rev
    let temperatures = Array.zeroCreate<int> 100
    Seq.fold2(fun rIdx c shift ->
        let robot = if c = '>' then 1 else -1
        let shift' = if shift = '>' then -1 else 1
        
        let rIdx' =
            match c with
            | '>' -> (rIdx + robot + shift') % 100
            | '<' -> ((rIdx + robot + shift') + 100) % 100
            | _ -> failwith "invalid movement"
        temperatures[rIdx'] <- temperatures[rIdx'] + 1
        rIdx'
    ) 0 wallSegmentsForLaser wallSegmentsForRobot |> ignore
    let (pos, v) =
        temperatures
        |> Array.indexed
        |> Array.maxBy snd
    (pos + 1) * v
