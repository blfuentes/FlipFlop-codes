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
    let input = ReadFileAsLines true 11 |> Seq.filter ((<>) System.Environment.NewLine)
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

    let growTree (tree: Dictionary<int, Rule>) (years: int) =
        let mutable currentYear = 0
        let growingTree = Queue<Rule>()
        growingTree.Enqueue(tree[0])
        while currentYear < years do
            let newRules = 
                [while growingTree.Count > 0 do
                    let rule = growingTree.Dequeue()
                    if rule.Left.IsSome then
                        yield tree[rule.Left.Value.From]
                    if rule.Right.IsSome then
                        yield tree[rule.Right.Value.From]
                    if rule.Above.IsSome then
                        yield tree[rule.Above.Value.From]
                ]
            newRules |> Seq.iter(fun r -> growingTree.Enqueue(r))

            currentYear <- currentYear + 1
        growingTree.Count

    rulesByTree |> Seq.map (fun (i, t) -> growTree t 5) |> Seq.sum

// Part 2
let SolvePart2 () =
    0

// Part 3
let SolvePart3 () =
    0