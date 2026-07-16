module Puzzle11

open LocalHelper
open System.Text.RegularExpressions
open System.Collections.Generic

type Rule = {
    From: int
    Left: Option<Rule>
    Right: Option<Rule>
    Above: Option<Rule>
}

// Part 1
let SolvePart1 () =
    let input = ReadFileAsLines false 11 |> Seq.filter ((<>) "")
    let chunks = input |> Seq.chunkBySize 2 |> Array.ofSeq
    let regexPatternTriple = @"(?:\d{2}|XX)(?: {2})(?:\d{2}|XX)(?: {2})(?:\d{2}|XX)"
    let regexPatternSingle = @"(?:\d{2}|XX)"
    let rulesByTree = seq {
        for (treeIdx, dna) in chunks |> Seq.indexed do
            let tree = Dictionary<int, Rule>()
            let (defUp, defDown) = (dna[0], dna[1])
            let upDef = Regex.Matches(defUp, regexPatternSingle)
            let leftRightDef = Regex.Matches(defDown, regexPatternTriple)
            for (above, lcr) in Seq.zip upDef leftRightDef do
                let leftRightDef = Regex.Matches(lcr.Value, regexPatternSingle)
                let fromSprout = int(leftRightDef.Item(1).Value)
                let leftSprout = 
                    if leftRightDef.Item(0).Value = "XX" then 
                        None 
                    else 
                        Some({ From = int(leftRightDef.Item(0).Value); Left = None; Right = None; Above = None })
                let rightSprout = 
                    if leftRightDef.Item(2).Value = "XX" then 
                        None
                    else Some({ From = int(leftRightDef.Item(2).Value); Left = None; Right = None; Above = None })
                let aboveSprout = 
                    if above.Value = "XX" then
                        None
                    else 
                        Some({ From = int(above.Value); Left = None; Right = None; Above = None })
                tree.Add(fromSprout, { From = fromSprout; Left = leftSprout; Right = rightSprout; Above = aboveSprout })
            yield (treeIdx, tree)
        }

    let producedEnergy (year: int) (sprouts: Queue<Rule*(int*int)>) (stems: HashSet<int*int>) =
        let stemsTo = stems |> Array.ofSeq
        //printfn "=== Year %d ===" year
        stemsTo
        |> Array.sumBy(fun (stemX, stemY) ->
            let heightEnergy = min 10 (abs(stemY - 1))
            let stemsAbove = (stemsTo |> Array.filter(fun (x, y) -> x = stemX && y < stemY)).Length
            let energyMultiplier = if stemsAbove > 2 then 0 else (3 - stemsAbove)
            let energy = heightEnergy * energyMultiplier
            //printfn "Stem at (%d, %d) produces %d (%d * %d)" stemX stemY energy heightEnergy energyMultiplier
            energy
        )

    let growTree (tree: Dictionary<int, Rule>) (years: int) =
        let mutable currentYear = 0
        let sprouts = Queue<Rule*(int*int)>()
        let stems = HashSet<int*int>()
        sprouts.Enqueue(tree[0], (0, 0))
        let mutable enoughEnergy = true
        while currentYear < years && enoughEnergy do
            let positions = 
                [while sprouts.Count > 0 do
                    let (rule, (x, y)) = sprouts.Dequeue()

                    if rule.Left.IsSome && not (stems.Contains (x-1, y)) then
                        stems.Add (x, y) |> ignore // convert sprout into stem if generates new sprout                        
                        yield (tree[rule.Left.Value.From], (x - 1, y))
                    if rule.Right.IsSome && not (stems.Contains (x+1, y)) then
                        stems.Add (x, y) |> ignore // convert sprout into stem if generates new sprout
                        yield (tree[rule.Right.Value.From], (x + 1, y))
                    if rule.Above.IsSome && not (stems.Contains (x, y - 1)) then
                        stems.Add (x, y) |> ignore // convert sprout into stem if generates new sprout
                        yield (tree[rule.Above.Value.From], (x, y - 1))
                ]
            positions
            |> List.groupBy snd
            |> List.map snd
            |> List.iter(fun compareRules ->
                let highestRule = compareRules |> List.maxBy(fun (r, _) -> r.From)
                sprouts.Enqueue(highestRule)
            )
            currentYear <- currentYear + 1       
            let requiredEnergy = sprouts.Count * 3 + stems.Count * 3
            let energy = producedEnergy currentYear sprouts stems
            enoughEnergy <- if currentYear < 5 || energy >= requiredEnergy then true else false
            //printfn "At year %d produces %d energy and requires %d" currentYear energy requiredEnergy
        let biologicalMass = (sprouts.Count + stems.Count)
        printfn "Tree died with %d years and biological of %d" currentYear biologicalMass
        biologicalMass

    rulesByTree |> Seq.map (fun (i, t) -> growTree t 100) |> Seq.sum

// Part 2
let SolvePart2 () =
    0

// Part 3
let SolvePart3 () =
    0