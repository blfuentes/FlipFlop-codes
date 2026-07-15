module Puzzle03

open LocalHelper

let lowerCase = ['a'..'z']
let upperCase = ['A'..'Z']
let numbers = ['0'..'9']

// strength part 1
let containsLowerCase (pass: string) =
    if lowerCase |> Seq.exists(fun c -> pass.Contains c) then 1 else 0

let containsUpperCase (pass: string) =
    if upperCase |> Seq.exists(fun c -> pass.Contains c) then 1 else 0

let containsNumber (pass: string) =
    if numbers |> Seq.exists(fun c -> pass.Contains c) then 1 else 0

// strength part 2
let containstOnlyNumber7 (pass: string) =
    let validNumbers = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '8'; '9']  |> Set.ofSeq
    (Set.intersect (pass.ToCharArray() |> Set.ofArray) validNumbers).Count = 0 && pass.Contains('7')

let moreThanThree (pass: string) =
    let mutable longestSoFar = 0    
    let mutable current = '%'
    let repeats = [
        for idx in 0..pass.Length do
            if idx = pass.Length then 
                yield longestSoFar + 1
            else
                if pass[idx] = current then 
                    longestSoFar <- longestSoFar + 1 
                else
                    yield longestSoFar + 1
                    longestSoFar <- 0
                current <- pass[idx]
        ]
    let max = repeats |> List.max
    if max > 2 then max else 0

let containsRedGreenBlue (pass: string) =
    pass.Contains("red") || pass.Contains("green") || pass.Contains("blue")

// Part 1
let SolvePart1 () =
    let passwords = ReadFileAsLines false 3
    let passwordStrengh (pass: string) =
        (containsLowerCase pass + containsUpperCase pass + containsNumber pass) * pass.Length
    
    passwords |> Seq.maxBy passwordStrengh

// Part 2
let SolvePart2 () =
    let passwords = ReadFileAsLines false 3
    let passwordStrengh (pass: string) =
        let sevenScore = if containstOnlyNumber7 pass then 7 else 0
        let moreThanThreeScore = moreThanThree pass
        let colorMultiplier = if containsRedGreenBlue pass then 3 else 1
        (containsLowerCase pass + 
            containsUpperCase pass + 
            containsNumber pass + 
            sevenScore +
            moreThanThreeScore * moreThanThreeScore) * colorMultiplier * pass.Length
    passwords |> Seq.maxBy passwordStrengh

// Part 3
let SolvePart3 () =
    let passwords = ReadFileAsLines false 3
    let passwordStrengh (pass: string) =
        let sevenScore = if containstOnlyNumber7 pass then 7 else 0
        let moreThanThreeScore = moreThanThree pass
        let colorMultiplier = if containsRedGreenBlue pass then 3 else 1
        (containsLowerCase pass + 
            containsUpperCase pass + 
            containsNumber pass + 
            sevenScore +
            moreThanThreeScore * moreThanThreeScore) * colorMultiplier * pass.Length
    let scores append =
        passwords |> Array.sumBy(fun p -> passwordStrengh (p+append))
    
    lowerCase @ upperCase @ numbers
    |> Seq.map (fun c -> scores (c.ToString()))
    |> Seq.max