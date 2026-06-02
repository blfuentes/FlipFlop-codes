module Puzzle05

// Part 1
let SolvePart1 =
    let tunel = LocalHelper.ReadFileAsText false 5
    let rec countSteps (t: char array) dir steps index =
        if index >= t.Length then
            steps
        else
            let charToFind = t[index]
            let first = t |> Array.findIndex _.Equals(charToFind)
            let second = t |> Array.findIndexBack _.Equals(charToFind)
            let nextIndex = if first = index then second else first
            countSteps t ((dir + 1) % 2) (steps + abs (nextIndex - index)) (nextIndex + 1)
    countSteps (tunel.ToCharArray()) 0 0 0

// Part 2
let SolvePart2 =
    let tunel = LocalHelper.ReadFileAsText false 5

    let visitedTunels =
        let d = System.Collections.Generic.Dictionary<char,bool>()
        tunel.ToCharArray()
        |> Array.iter (fun c -> d[c] <- false)
        d

    let rec countSteps (t: char array) dir steps index =
        if index >= t.Length then
            sprintf "%s" (visitedTunels |> Seq.filter(fun kvp -> not kvp.Value) |> Seq.map _.Key |> Seq.toArray |> System.String)
        else
            let charToFind = t[index]
            visitedTunels[charToFind] <- true
            let first = t |> Array.findIndex _.Equals(charToFind)
            let second = t |> Array.findIndexBack _.Equals(charToFind)
            let nextIndex = if first = index then second else first
            countSteps t ((dir + 1) % 2) (steps + abs (nextIndex - index)) (nextIndex + 1)
    countSteps (tunel.ToCharArray()) 0 0 0

// Part 3
let SolvePart3 =
    let tunel = LocalHelper.ReadFileAsText false 5
    let rec countSteps (t: char array) dir steps index =
        if index >= t.Length then
            steps
        else
            let charToFind = t[index]
            let first = t |> Array.findIndex _.Equals(charToFind)
            let second = t |> Array.findIndexBack _.Equals(charToFind)
            let nextIndex = if first = index then second else first

            countSteps t ((dir + 1) % 2) ((steps + (abs (nextIndex - index)) * if charToFind < 'a' then -1 else 1)) (nextIndex + 1)
    countSteps (tunel.ToCharArray()) 0 0 0