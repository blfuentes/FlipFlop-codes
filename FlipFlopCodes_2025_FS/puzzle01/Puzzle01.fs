module Puzzle01

open LocalHelper

// Part 1
let SolvePart1 =
    let content = ReadFileAsText false 1
    let noba = content.Replace("ba", "")
    let nona = noba.Replace("na", "")
    let none = nona.Replace("ne", "")
    (content.Length - none.Length) / 2

// Part 2
let SolvePart2 =
    let lines = ReadFileAsLines false 1
    lines
    |> Array.sumBy(fun line ->
        let noba = line.Replace("ba", "")
        let nona = noba.Replace("na", "")
        let none = nona.Replace("ne", "")
        let matches = (line.Length - none.Length) / 2
        if matches % 2 = 0 then 
            matches 
        else 
            0
    )

// Part 3
let SolvePart3 =
    let lines = ReadFileAsLines false 1
    lines
    |> Array.sumBy(fun line ->
        let noba = line.Replace("ba", "")
        let nona = noba.Replace("na", "")
        let none = nona.Replace("ne", "")
        if none.Length < nona.Length then
            0
        else
            (line.Length - none.Length) / 2
    )