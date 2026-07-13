module Puzzle08

open System.Collections.Generic
open LocalHelper
open System.Text

let isTest = true

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
        rules.TryAdd(a+b, (a + cleaned[2..] + b)) |> ignore
        rules.TryAdd(b+a, (b + cleaned[2..] + a)) |> ignore
    
    let rec generate (stoat: string) (count: int) (goal: int) =
        if count = goal then
            stoat.Length
        else
            let sb = StringBuilder()
            let parts =
                stoat.ToCharArray() 
                |> Seq.pairwise 
                |> Seq.map(fun (a,b) -> System.String [|a; b|])  
                |> Seq.map (fun r -> rules[r])
            if parts |> Seq.length > 1 then
                parts
                |> Seq.iteri(fun i p -> 
                    if i = 0 then 
                        sb.Append (p.Substring(0, p.Length - 1)) |> ignore
                    else
                        sb.Append p[1..] |> ignore
                )
            else
                sb.Append (parts |> Seq.head) |> ignore
            
            generate (sb.ToString()) (count + 1) goal

    generate "AB" 0 7

// Part 3
let SolvePart3 =
    0