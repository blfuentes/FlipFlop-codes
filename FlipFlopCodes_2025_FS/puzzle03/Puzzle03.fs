module Puzzle03

open LocalHelper

// Part 1
let SolvePart1 =
    let content = ReadFileAsLines false 3
    let colors = 
        content |> Seq.map(fun line ->
            let parts = line.Split(',') |> Seq.map int |> Seq.toArray
            System.Drawing.Color.FromArgb(parts[0], parts[1], parts[2])
        )
    colors
    |> Seq.groupBy id
    |> Seq.sortByDescending (fun (_, group) -> Seq.length group)
    |> Seq.map(fun (color, group) -> sprintf "%d,%d,%d" color.R color.G color.B)
    |> Seq.head

// Part 2
let SolvePart2 =
    let content = ReadFileAsLines false 3
    let colors = 
        content |> Seq.map(fun line ->
            let parts = line.Split(',') |> Seq.map int |> Seq.toArray
            System.Drawing.Color.FromArgb(parts[0], parts[1], parts[2])
        )
    let labeled =
        (colors
        |> Seq.groupBy(fun c ->
            if c.R = c.G || c.R = c.B || c.G = c.B then "Special"
            elif c.R > c.G && c.R > c.B then "Red"
            elif c.G > c.R && c.G > c.B then "Green"
            elif c.B > c.R && c.B > c.G then "Blue"
            else "-"
        )
        |> dict)
    labeled.Item("Green") |> Seq.length

// Part 3
let SolvePart3 =
    let content = ReadFileAsLines false 3
    let colors = 
        content |> Seq.map(fun line ->
            let parts = line.Split(',') |> Seq.map int |> Seq.toArray
            System.Drawing.Color.FromArgb(parts[0], parts[1], parts[2])
        )
    let labeled =
        (colors
        |> Seq.groupBy(fun c ->
            if c.R = c.G || c.R = c.B || c.G = c.B then "Special"
            elif c.R > c.G && c.R > c.B then "Red"
            elif c.G > c.R && c.G > c.B then "Green"
            elif c.B > c.R && c.B > c.G then "Blue"
            else "-"
        )
        |> dict)
    labeled
    |> Seq.sumBy(fun c ->
       match c.Key with
        | "Red" -> Seq.length c.Value * 5
        | "Green" -> Seq.length c.Value * 2
        | "Blue" -> Seq.length c.Value * 4
        | "Special" -> Seq.length c.Value * 10
        | _ -> 0
    )