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

let private parseTrees isTest =
    let input = ReadFileAsLines isTest 11 |> Seq.filter ((<>) "")
    let chunks = input |> Seq.chunkBySize 2
    let regexPatternTriple = @"(?:\d{2}|XX)(?: {2})(?:\d{2}|XX)(?: {2})(?:\d{2}|XX)"
    let regexPatternSingle = @"(?:\d{2}|XX)"

    [|for dna in chunks do
        let tree = Dictionary<int, Rule>()
        let upDef = Regex.Matches(dna[0], regexPatternSingle)
        let leftRightDef = Regex.Matches(dna[1], regexPatternTriple)

        for above, lcr in Seq.zip upDef leftRightDef do
            let definitions = Regex.Matches(lcr.Value, regexPatternSingle)
            let sprout value =
                if value = "XX" then None
                else Some { From = int value; Left = None; Right = None; Above = None }

            let fromSprout = int definitions[1].Value
            tree.Add(fromSprout, {
                From = fromSprout
                Left = sprout definitions[0].Value
                Right = sprout definitions[2].Value
                Above = sprout above.Value
            })

        yield tree|]

let private producedEnergy (allStems: HashSet<int * int>) (treeStems: HashSet<int * int>) =
    let stems = allStems |> Array.ofSeq

    treeStems
    |> Seq.sumBy (fun (stemX, stemY) ->
        let heightEnergy = min 10 (1 - stemY)
        let stemsAbove =
            stems
            |> Array.sumBy (fun (x, y) -> if x = stemX && y < stemY then 1 else 0)
        let energyMultiplier = max 0 (3 - stemsAbove)
        heightEnergy * energyMultiplier)

type private TreeState = {
    Rules: Dictionary<int, Rule>
    Sprouts: Queue<Rule * (int * int)>
    Stems: HashSet<int * int>
    mutable Age: int
    mutable Alive: bool
}

let private energyByStem (allStems: HashSet<int * int>) =
    let result = Dictionary<int * int, int>()

    allStems
    |> Seq.groupBy fst
    |> Seq.iter (fun (_, column) ->
        column
        |> Seq.sortBy snd
        |> Seq.iteri (fun stemsAbove (x, y) ->
            let energyMultiplier = max 0 (3 - stemsAbove)
            result[(x, y)] <- min 10 (1 - y) * energyMultiplier))

    result

// Part 1
let SolvePart1 () =
    let growTree (tree: Dictionary<int, Rule>) =
        let mutable currentYear = 0
        let sprouts = Queue<Rule * (int * int)>()
        let stems = HashSet<int * int>()
        sprouts.Enqueue(tree[0], (0, 0))
        let mutable enoughEnergy = true

        while currentYear < 100 && enoughEnergy do
            let currentSprouts =
                [while sprouts.Count > 0 do
                    yield sprouts.Dequeue()]

            currentSprouts
            |> List.iter (fun (_, position) -> stems.Add position |> ignore)

            let positions = 
                [for (rule, (x, y)) in currentSprouts do
                    if rule.Left.IsSome && not (stems.Contains (x-1, y)) then
                        yield (tree[rule.Left.Value.From], (x - 1, y))
                    if rule.Right.IsSome && not (stems.Contains (x+1, y)) then
                        yield (tree[rule.Right.Value.From], (x + 1, y))
                    if rule.Above.IsSome && not (stems.Contains (x, y - 1)) then
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
            let requiredEnergy = (sprouts.Count + stems.Count) * 3
            enoughEnergy <- currentYear < 5 || producedEnergy stems stems >= requiredEnergy

        sprouts.Count + stems.Count

    parseTrees false |> Array.sumBy growTree

// Part 2
let SolvePart2 () =
    let trees =
        parseTrees false
        |> Array.mapi (fun treeIndex rules ->
            let sprouts = Queue<Rule * (int * int)>()
            sprouts.Enqueue(rules[0], (treeIndex * 10, 0))
            {
                Rules = rules
                Sprouts = sprouts
                Stems = HashSet<int * int>()
                Age = 0
                Alive = true
            })

    let occupied = HashSet<int * int>()
    trees |> Array.iteri (fun treeIndex _ -> occupied.Add((treeIndex * 10, 0)) |> ignore)

    while trees |> Array.exists _.Alive do
        for tree in trees do
            if tree.Alive then
                let currentSprouts =
                    [while tree.Sprouts.Count > 0 do
                        yield tree.Sprouts.Dequeue()]

                currentSprouts
                |> List.iter (fun (_, position) -> tree.Stems.Add position |> ignore)

                [for rule, (x, y) in currentSprouts do
                    for child, position in [rule.Left, (x - 1, y); rule.Right, (x + 1, y); rule.Above, (x, y - 1)] do
                        if child.IsSome && not (occupied.Contains position) then
                            yield tree.Rules[child.Value.From], position]
                |> List.groupBy snd
                |> List.map (snd >> List.maxBy (fun (rule, _) -> rule.From))
                |> List.iter (fun sprout ->
                    tree.Sprouts.Enqueue sprout
                    occupied.Add(snd sprout) |> ignore)

                tree.Age <- tree.Age + 1

        let allStems = HashSet<int * int>()
        trees |> Array.iter (fun tree -> allStems.UnionWith tree.Stems)
        let stemEnergy = energyByStem allStems

        for tree in trees do
            if tree.Alive then
                let biologicalMass = tree.Stems.Count + tree.Sprouts.Count
                let produced = tree.Stems |> Seq.sumBy (fun position -> stemEnergy[position])
                let hasEnoughEnergy = tree.Age < 5 || produced >= biologicalMass * 3
                tree.Alive <- tree.Age < 100 && hasEnoughEnergy

    trees |> Array.sumBy (fun tree -> tree.Stems.Count + tree.Sprouts.Count)

// Part 3
let SolvePart3 () =
    0