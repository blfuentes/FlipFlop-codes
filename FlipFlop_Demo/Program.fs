open System
open System.IO

let isTest = false

let filepath = if isTest then "test.txt" else "input.txt"
let content = File.ReadAllLines(filepath) |> Array.map int

// Part 1
content |> Array.sum
|> printfn "Demo part 1: %d"

// Part 2
int(Math.Round(float(content |> Array.sum) / float(content.Length), 1, MidpointRounding.AwayFromZero))
|> printfn "Demo part 2: %d"

// Part 3
let mostFrequentNumber = content |> Array.groupBy id |> Array.sortByDescending (fun (_, group) -> group.Length) |> Array.head |> fst
let leastFrequentDigit = File.ReadAllText(filepath) |> Seq.groupBy id |> Seq.sortBy (fun (_, group) -> Seq.length group) |> Seq.head |> fst
printfn "Demo part3: %d" (mostFrequentNumber * 10 + int leastFrequentDigit - int '0')