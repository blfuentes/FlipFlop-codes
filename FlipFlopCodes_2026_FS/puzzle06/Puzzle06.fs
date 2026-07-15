module Puzzle06

open System.Collections.Generic

type LightState =
| High
| Low
| Off

type ComponentType =
| Gear
| Light
| BluetoohInput
| BluetoohOutput
| Other

type RotationDir =
| CW
| CCW
| Stop

type Component = {
    Key: char
    Row: int
    Col: int
    Type: ComponentType
    Direction: RotationDir
}

// Part 1
let SolvePart1 () =
    let content = LocalHelper.ReadFileAsLines false 6
    let (maxRows, maxCols) = (content.Length - 1, content[0].Length - 1)
    let mutable startGear = (0, 0)
    let gears = Dictionary<(int*int), Component>()
    let lights = Dictionary<(int*int), Component>()
    let mim = Array2D.init maxRows maxCols (fun r c ->
        let symbol = content[r][c]
        let cType = 
            match symbol with
            | 'S' -> 
                startGear <- (r, c)
                Gear
            | '#' ->
                Gear
            | '*' ->
                Light
            | _ -> 
                Other
        let tmpComp = { Key = symbol; Row = r; Col = c; Type = cType; Direction = if symbol = 'S' then CCW else Stop }                
        if tmpComp.Type.IsGear then
            gears.Add((r,c), tmpComp)
        elif tmpComp.Type.IsLight then
            lights.Add((r, c), tmpComp)
        tmpComp
    )
    
    let visited = HashSet<int*int>()
    let gearsToRotate = Queue<Component>()
    let startPos = (fst startGear, snd startGear)
    gearsToRotate.Enqueue(gears[startPos])
    visited.Add(startPos) |> ignore
    let directions = [(-1, 0); (0, 1); (1, 0); (0, -1)]
    while gearsToRotate.Count > 0 do
        let rotating = gearsToRotate.Dequeue()
        for (dr, dc) in directions do
            let pos = (rotating.Row + dr, rotating.Col + dc)            
            let mutable gear = { Key = '-'; Row = 0; Col = 0; Type = Other; Direction = Stop }
            match gears.TryGetValue(pos, &gear) && not (visited.Contains pos) with
            | true ->
                gear <- { gear with Direction = if rotating.Direction.IsCCW then CW else CCW }
                gearsToRotate.Enqueue(gear)
                gears[pos] <- gear
            | false ->
                ignore()
            visited.Add pos |> ignore
    
    //for row in 0..maxRows do
    //    for col in 0..maxCols do
    //        let symbol =
    //            if gears.ContainsKey((row, col)) then
    //                match gears[(row, col)].Direction with
    //                | CCW -> 'L'
    //                | CW -> 'R'
    //                | _ -> failwith "not valid movement"
    //            elif lights.ContainsKey((row, col)) then
    //                '*'
    //            else
    //                '.'
    //        printf "%c" symbol
    //    printfn ""

    let sortedLights =
        lights 
        |> Seq.sortBy(fun kvp -> kvp.Key)
        |> Seq.map(fun kvp ->
                let nextToCW =
                    [(-1, 0); (0, 1); (1, 0); (0, -1)]
                    |> Seq.exists(fun (dr, dc) ->
                        let pos = (kvp.Value.Row + dr, kvp.Value.Col + dc)
                        gears.ContainsKey pos && gears[pos].Direction.IsCW
                    )
                let nextToCCW =
                    [(-1, 0); (0, 1); (1, 0); (0, -1)]
                    |> Seq.exists(fun (dr, dc) ->
                        let pos = (kvp.Value.Row + dr, kvp.Value.Col + dc)
                        gears.ContainsKey pos && gears[pos].Direction.IsCCW
                    )
                if nextToCW then 
                    High
                elif nextToCCW then
                    Low
                else
                    Off            
        )
        |> Seq.filter ((<>) Off)
        |> Seq.map(fun l -> if l.IsHigh then "1" else "0")
    System.Convert.ToUInt64((String.concat "" sortedLights), 2)
                

// Part 2
let SolvePart2 () =
    let content = LocalHelper.ReadFileAsLines false 6
    let (maxRows, maxCols) = (content.Length - 1, content[0].Length - 1)
    let mutable startGear = (0, 0)
    let gears = Dictionary<(int*int), Component>()
    let lights = Dictionary<(int*int), Component>()
    let bluetooths = Dictionary<(int*int), Component>()
    let mim = Array2D.init maxRows maxCols (fun r c ->
        let symbol = content[r][c]
        let cType = 
            match symbol with
            | 'S' -> 
                startGear <- (r, c)
                Gear
            | '#' ->
                Gear
            | '3' ->
                Gear
            | '*' ->
                Light
            | input when (['a'..'z'] |> Seq.contains input) ->
                BluetoohInput
            | output when (['A'..'Z'] |> Seq.contains output) ->
                BluetoohOutput
            | _ -> 
                Other
        let tmpComp = { Key = symbol; Row = r; Col = c; Type = cType; Direction = if symbol = 'S' then CCW else Stop }                
        if tmpComp.Type.IsGear then
            gears.Add((r,c), tmpComp)
        elif tmpComp.Type.IsLight then
            lights.Add((r, c), tmpComp)
        elif tmpComp.Type.IsBluetoohInput || tmpComp.Type.IsBluetoohOutput then
            bluetooths.Add((r, c), tmpComp)
        tmpComp
    )
    
    let visited = HashSet<int*int>()
    let gearsToRotate = Queue<Component>()
    let startPos = (fst startGear, snd startGear)
    gearsToRotate.Enqueue(gears[startPos])
    visited.Add(startPos) |> ignore
    let directions = [(-1, 0); (0, 1); (1, 0); (0, -1)]
    while gearsToRotate.Count > 0 do
        let rotating = gearsToRotate.Dequeue()
        for (dr, dc) in directions do
            let pos = (rotating.Row + dr, rotating.Col + dc)            
            let mutable gear = { Key = '-'; Row = 0; Col = 0; Type = Other; Direction = Stop }
            match gears.TryGetValue(pos, &gear) && not (visited.Contains pos) with
            | true ->
                gear <- { gear with Direction = if rotating.Direction.IsCCW then CW else CCW }
                gearsToRotate.Enqueue(gear)
                gears[pos] <- gear
            | false ->
                let mutable bluetoothinput = { Key = '-'; Row = 0; Col = 0; Type = Other; Direction = Stop }
                match bluetooths.TryGetValue(pos, &bluetoothinput) && 
                    not (visited.Contains pos) &&
                    bluetoothinput.Type.IsBluetoohInput with
                | true ->
                    let output = bluetooths |> Seq.find(fun kvp -> kvp.Value.Key = char(int(bluetoothinput.Key) - 32))
                    let outputBluetooth = { output.Value with Direction = rotating.Direction }
                    gearsToRotate.Enqueue(outputBluetooth)
                | false ->
                    ignore()
            visited.Add pos |> ignore
    
    //for row in 0..maxRows do
    //    for col in 0..maxCols do
    //        let symbol =
    //            if gears.ContainsKey((row, col)) then
    //                match gears[(row, col)].Direction with
    //                | CCW -> 'L'
    //                | CW -> 'R'
    //                | _ -> failwith "not valid movement"
    //            elif lights.ContainsKey((row, col)) then
    //                '*'
    //            elif bluetooths.ContainsKey((row, col)) then
    //                bluetooths[(row, col)].Key
    //            else
    //                '.'
    //        printf "%c" symbol
    //    printfn ""

    let sortedLights =
        lights 
        |> Seq.sortBy(fun kvp -> kvp.Key)
        |> Seq.map(fun kvp ->
                let nextToCW =
                    [(-1, 0); (0, 1); (1, 0); (0, -1)]
                    |> Seq.exists(fun (dr, dc) ->
                        let pos = (kvp.Value.Row + dr, kvp.Value.Col + dc)
                        gears.ContainsKey pos && gears[pos].Direction.IsCW
                    )
                let nextToCCW =
                    [(-1, 0); (0, 1); (1, 0); (0, -1)]
                    |> Seq.exists(fun (dr, dc) ->
                        let pos = (kvp.Value.Row + dr, kvp.Value.Col + dc)
                        gears.ContainsKey pos && gears[pos].Direction.IsCCW
                    )
                if nextToCW then 
                    High
                elif nextToCCW then
                    Low
                else
                    Off            
        )
        |> Seq.filter ((<>) Off)
        |> Seq.map(fun l -> if l.IsHigh then "1" else "0")
    System.Convert.ToUInt64((String.concat "" sortedLights), 2)

// Part 3
let SolvePart3 () =
    let content = LocalHelper.ReadFileAsLines false 6
    let (maxRows, maxCols) = (content.Length - 1, content[0].Length - 1)
    let mutable startGear = (0, 0)
    let gears = Dictionary<(int*int), Component>()
    let lights = Dictionary<(int*int), Component>()
    let bluetooths = Dictionary<(int*int), Component>()
    let mim = Array2D.init maxRows maxCols (fun r c ->
        let symbol = content[r][c]
        let cType = 
            match symbol with
            | 'S' -> 
                startGear <- (r, c)
                Gear
            | '#' ->
                Gear
            | '3' ->
                Gear
            | '*' ->
                Light
            | input when (['a'..'z'] |> Seq.contains input) ->
                BluetoohInput
            | output when (['A'..'Z'] |> Seq.contains output) ->
                BluetoohOutput
            | _ -> 
                Other
        let tmpComp = { Key = symbol; Row = r; Col = c; Type = cType; Direction = if symbol = 'S' then CCW else Stop }                
        if tmpComp.Type.IsGear then
            gears.Add((r,c), tmpComp)
        elif tmpComp.Type.IsLight then
            lights.Add((r, c), tmpComp)
        elif tmpComp.Type.IsBluetoohInput || tmpComp.Type.IsBluetoohOutput then
            bluetooths.Add((r, c), tmpComp)
        tmpComp
    )
    
    let visited = HashSet<int*int>()
    let gearsToRotate = Stack<char*Component>()
    let startPos = (fst startGear, snd startGear)
    
    gearsToRotate.Push('s', gears[startPos])
    visited.Add(startPos) |> ignore
    
    let directions = [(-1, 0); (0, 1); (1, 0); (0, -1)]

    let sections = Dictionary<char, HashSet<int*int>>()
    let connectedbluetooths = HashSet<char*char>()

    while gearsToRotate.Count > 0 do
        let (p, rotating) = gearsToRotate.Pop()
        for (dr, dc) in directions do
            let pos = (rotating.Row + dr, rotating.Col + dc)            
            let mutable gear = { Key = '-'; Row = 0; Col = 0; Type = Other; Direction = Stop }
            match gears.TryGetValue(pos, &gear) && not (visited.Contains pos) with
            | true ->
                gear <- { gear with Direction = if rotating.Direction.IsCCW then CW else CCW }
                gears[pos] <- gear
                gearsToRotate.Push(p, gear)
                if p <> 's' then
                    sections[p].Add pos |> ignore
            | false ->
                let mutable bluetoothinput = { Key = '-'; Row = 0; Col = 0; Type = Other; Direction = Stop }
                match bluetooths.TryGetValue(pos, &bluetoothinput) && 
                    not (visited.Contains pos) &&
                    bluetoothinput.Type.IsBluetoohInput with
                | true ->
                    let outputKey = char(int(bluetoothinput.Key) - 32)
                    let output = bluetooths |> Seq.find(fun kvp -> kvp.Value.Key = outputKey)
                    let outputBluetooth = { output.Value with Direction = rotating.Direction }
                    gearsToRotate.Push((outputKey, outputBluetooth))
                    sections.TryAdd(outputKey, new HashSet<int*int>()) |> ignore
                    connectedbluetooths.Add (outputKey, p) |> ignore
                | false ->
                    ignore()
            visited.Add pos |> ignore
 
    let isPrime n =
        if n <= 1 then false
        elif n = 2 then true
        elif n % 2 = 0 then false
        else
            let rec checkDivisor d =
                if d * d > n then true
                elif n % d = 0 then false
                else checkDivisor (d + 2)
        
            checkDivisor 3     


    let disableSections = HashSet<char>()
    sections
    |> Seq.iter(fun kvp ->
        if isPrime kvp.Value.Count then
            disableSections.Add kvp.Key |> ignore
    )

    let mutable added = true
    while added do
        match connectedbluetooths |> Seq.tryFind(fun (f, t) -> disableSections.Contains t) with
        | Some (f, t) ->
            connectedbluetooths.Remove((f, t)) |> ignore
            added <- disableSections.Add f
        | None ->
            added <- false

    let disableSections = 
        sections 
        |> Seq.filter(fun kvp -> disableSections.Contains kvp.Key)
        |> Seq.collect(fun kvp -> kvp.Value)

    //for row in 0..maxRows do
    //    for col in 0..maxCols do
    //        let symbol =
    //            if disableSections |> Seq.contains (row, col) then
    //                '@'
    //            elif gears.ContainsKey((row, col)) then
    //                match gears[(row, col)].Direction with
    //                | CCW -> 'L'
    //                | CW -> 'R'
    //                | _ -> failwith "not valid movement"
    //            elif lights.ContainsKey((row, col)) then
    //                '*'
    //            elif bluetooths.ContainsKey((row, col)) then
    //                bluetooths[(row, col)].Key
    //            else
    //                '.'
    //        printf "%c" symbol
    //    printfn ""

    let sortedLights =
        lights 
        |> Seq.sortBy(fun kvp -> kvp.Key)
        |> Seq.map(fun kvp ->
                let nextToCW =
                    [(-1, 0); (0, 1); (1, 0); (0, -1)]
                    |> Seq.exists(fun (dr, dc) ->
                        let pos = (kvp.Value.Row + dr, kvp.Value.Col + dc)

                        not(disableSections |> Seq.contains pos) &&
                        gears.ContainsKey pos && 
                        gears[pos].Direction.IsCW
                    )
                let nextToCCW =
                    [(-1, 0); (0, 1); (1, 0); (0, -1)]
                    |> Seq.exists(fun (dr, dc) ->
                        let pos = (kvp.Value.Row + dr, kvp.Value.Col + dc)

                        not(disableSections |> Seq.contains pos) &&
                        gears.ContainsKey pos && gears[pos].Direction.IsCCW
                    )
                if nextToCW then 
                    High
                elif nextToCCW then
                    Low
                else
                    Off            
        )
        |> Seq.filter ((<>) Off)
        |> Seq.map(fun l -> if l.IsHigh then "1" else "0")
    System.Convert.ToUInt64((String.concat "" sortedLights), 2)