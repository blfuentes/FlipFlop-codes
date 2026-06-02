module Puzzle05

// Part 1
let SolvePart1 =
    let tunel = LocalHelper.ReadFileAsText true 5
    let rec countSteps (t: char array) dir steps index =
        if index >= t.Length then
            index
        else
            let charToFind = t[index]
            let nextIndex =
                match dir with
                | 0 -> t |> Array.findIndexBack (fun c -> c = charToFind)
                | 1 -> t |> Array.findIndex (fun c -> c = charToFind)
                | _ -> failwith "Invalid direction"
            countSteps t ((dir + 1) % 2) (steps + abs (nextIndex - index)) (nextIndex + 1)
    countSteps (tunel.ToCharArray()) 0 0 0

// Part 2
let SolvePart2 =
    0

// Part 3
let SolvePart3 =
    0