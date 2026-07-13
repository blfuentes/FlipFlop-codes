module Puzzle08

open System.Collections.Generic
open LocalHelper
open System.Text

let isTest = false

// Part 1
let SolvePart1 =
    let rules = new Dictionary<string,string>()
    for l in ReadFileAsLines isTest 8 do
        rules.TryAdd(l[..0], l[1..].Replace(" ", "")) |> ignore
    
    let rec generate (stoat: string) (count: int) (goal: int) =
        if count = goal then
            stoat.Length
        else
            let sb = StringBuilder()
            stoat |> Seq.map string |> Seq.iter(fun v -> sb.Append(rules[v]) |> ignore)
            generate (sb.ToString()) (count + 1) goal

    generate "AB" 0 7
    

// Part 2
let SolvePart2 =
    let rules = new Dictionary<string,string>()
    for l in ReadFileAsLines isTest 8 do
        let cleaned = l.Replace(" ", "")
        let (a, b) = (cleaned[0].ToString(), cleaned[1].ToString())
        rules.TryAdd(a+b, cleaned[2..]) |> ignore
        rules.TryAdd(b+a, cleaned[2..]) |> ignore
    
    let rec generate (stoat: string) (count: int) (goal: int) =
        if count = goal then
            stoat.Length
        else
            let sb = StringBuilder()
            let parts =
                stoat.ToCharArray() 
                |> Seq.pairwise 
            if parts |> Seq.length = 1 then
                parts
                |> Seq.iteri(fun i (a,b) -> 
                    let key = System.String [|a; b|]
                    sb.Append a |> ignore
                    sb.Append rules[key] |> ignore
                    sb.Append b |> ignore
                )
            else
                parts
                |> Seq.iteri(fun i (a,b) -> 
                    let key = System.String [|a; b|]
                    sb.Append a |> ignore
                    sb.Append rules[key] |> ignore
                    if i = (parts |> Seq.length) - 1 then
                        sb.Append b |> ignore
                )
            
            generate (sb.ToString()) (count + 1) goal

    generate "AB" 0 7

// Part 3
let SolvePart3 =
    let rules = new Dictionary<string,string>()
    for l in ReadFileAsLines isTest 8 do
        let cleaned = l.Replace(" ", "")
        let (a, b) = (cleaned[0].ToString(), cleaned[1].ToString())
        rules.TryAdd(a+b, cleaned[2..]) |> ignore
        rules.TryAdd(b+a, cleaned[2..]) |> ignore
    
    // memoize the lengths of mid generated mid parts
    let memo = new Dictionary<struct (char * char * int), int64>()

    let rec expand (a: char) (b: char) (steps: int) : int64 =
        if steps = 0 then
            1L
        else
            let cacheKey = struct (a, b, steps)
            match memo.TryGetValue cacheKey with
            | true, v -> v
            | _ ->
                let key = System.String [| a; b |]
                let mid = rules[key]
                // ab -> a+mid+b, so loop expanding each mid part
                let chars = Array.append [| a |] (Array.append (mid.ToCharArray()) [| b |])
                let mutable total = 0L
                for i in 0 .. chars.Length - 2 do
                    total <- total + expand chars[i] chars[i + 1] (steps - 1)
                memo[cacheKey] <- total
                total

    
    expand 'A' 'B' 21 + 1L // add 1 because of the missing right side on expanding