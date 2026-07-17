module Puzzle12

open LocalHelper
open System.Text.RegularExpressions

let cardSize = 5
type PlayingCard = {
        Card: int[,]
        ValidRows: int list
        ValidCols: int list
        ValidDiagL: bool
        ValidDiagR: bool
    }
    
// Part 1
let SolvePart1 () =
    let buildCard (numbers: int seq) =
        let card = Array2D.zeroCreate<int> cardSize cardSize
        numbers 
        |> Seq.chunkBySize cardSize
        |> Seq.iteri(fun row values -> 
            values
            |> Seq.iteri(fun col value ->
                card[row, col] <- value
            )
        )
        { Card = card; ValidRows = [0..cardSize-1]; ValidCols = [0..cardSize-1]; ValidDiagL = true; ValidDiagR = true }

    let checkCard (playingCard: PlayingCard) =
        let rows =
            playingCard.ValidRows |> Seq.filter(fun row ->
                let values = playingCard.Card[row, *] 
                values |> Array.forall ((=) -1)
            ) |> List.ofSeq
        let cols =
            playingCard.ValidCols |> Seq.filter(fun col ->
                let values = playingCard.Card[*, col] 
                values |> Array.forall ((=) -1)
            ) |> List.ofSeq
        let diagValuesL = [|for r in 0..cardSize-1 do yield playingCard.Card[r, r]|]
        let diagL = playingCard.ValidDiagL &&  (diagValuesL |> Array.forall((=) -1))
        let diagValuesR = [|for r in 0..cardSize-1 do yield playingCard.Card[r, cardSize-1-r]|]
        let diagR = playingCard.ValidDiagR &&  (diagValuesR |> Array.forall((=) -1))
        (rows, cols, diagL, diagR)

    let markNumber (number: int) (playingCard: PlayingCard) =
        for r in 0..(playingCard.Card.GetLength(0)-1) do
            for c in 0..(playingCard.Card.GetLength(1)-1) do
                if playingCard.Card[r, c] = number then
                    playingCard.Card[r, c] <- -1
    let markCard (rows: int List) (cols: int List) ((diagL, diagR): bool*bool) (playingCard: PlayingCard) =
        { 
            playingCard with 
                ValidRows = (playingCard.ValidRows |> List.filter (fun r -> not (rows |> List.contains r)));
                ValidCols = (playingCard.ValidCols |> List.filter (fun c -> not (cols |> List.contains c)));
                ValidDiagL = diagL;
                ValidDiagR = diagR
        }

    let printBingoCard (playingCard: PlayingCard) =
        for r in 0..(playingCard.Card.GetLength(0)-1) do
            for c in 0..(playingCard.Card.GetLength(1)-1) do
                if playingCard.Card[r, c] = -1 then
                    printf "%4s" "XX"
                else
                    printf "%4d" playingCard.Card[r, c]
            printfn ""
    let rec signNumber (numbers: int list) (cards: PlayingCard array) (numberOfBingo: int) =
        match numbers with
        | n::rest ->
            //printfn "Calling out number %d" n
            cards |> Seq.iter(fun c -> markNumber n c)
            //if n = 49 then
            //    for (i, card) in cards |> Array.indexed do
            //        printBingoCard card
            //        printfn ""

            let mutable newbingos = 0
            for (i, card) in cards |> Array.indexed do
                let (brows, bcols, diagL, diagR) = checkCard card
                newbingos <- newbingos + brows.Length + bcols.Length + (if diagL then 1 else 0) + (if diagR then 1 else 0)
                let nDL = 
                    if card.ValidDiagL then
                        if diagL then false else true
                    else
                        false
                let nDR = 
                    if card.ValidDiagR then
                        if diagR then false else true
                    else
                        false
                cards[i] <- markCard brows bcols (nDL, nDR) card
            let newNumerOfBingo = numberOfBingo + newbingos
            if newNumerOfBingo >= 5 then
                //for (i, card) in cards |> Array.indexed do
                //    printBingoCard card
                //    printfn ""
                n
            else
                signNumber rest cards newNumerOfBingo
        | [] -> failwith "no found 5 bingos"

    let input = ReadFileAsText false 12
    let splitter = String.replicate 2 System.Environment.NewLine
    let digitRegex = @"\d+"
    let (callednumbers, bingocardnumbers) = (
            Regex.Matches(input.Split(splitter)[0], digitRegex) |> Seq.map _.Value |> Seq.map int |> List.ofSeq,
            Regex.Matches(input.Split(splitter)[1], digitRegex) |> Seq.map _.Value |> Seq.map int |> Seq.chunkBySize 25
        )
    let cards = bingocardnumbers |> Seq.map buildCard |> Array.ofSeq
    signNumber callednumbers cards 0

// Part 2
let SolvePart2 () =
    let buildCube (numbers: int array) =
        let cube = Array3D.zeroCreate<int> cardSize cardSize cardSize
        numbers
        |> Array.iteri (fun index value ->
            let z = index / (cardSize * cardSize)
            let x = (index / cardSize) % cardSize
            let y = index % cardSize
            cube[z, x, y] <- value)
        cube

    let directions =
        [| for dz in -1..1 do
               for dx in -1..1 do
                   for dy in -1..1 do
                       if (dz > 0 || (dz = 0 && dx > 0) || (dz = 0 && dx = 0 && dy > 0)) then
                           yield dz, dx, dy |]

    let lines =
        [| for dz, dx, dy in directions do
               for z in 0..cardSize - 1 do
                   for x in 0..cardSize - 1 do
                       for y in 0..cardSize - 1 do
                           let endZ = z + (cardSize - 1) * dz
                           let endX = x + (cardSize - 1) * dx
                           let endY = y + (cardSize - 1) * dy
                           if endZ >= 0 && endZ < cardSize
                              && endX >= 0 && endX < cardSize
                              && endY >= 0 && endY < cardSize then
                               yield [| for offset in 0..cardSize - 1 ->
                                              z + offset * dz, x + offset * dx, y + offset * dy |] |]

    let input = ReadFileAsText false 12
    let sections = Regex.Split(input.Trim(), @"\r?\n\s*\r?\n")
    let calledNumbers =
        Regex.Matches(sections[0], @"\d+")
        |> Seq.map (fun value -> int value.Value)
        |> List.ofSeq
    let cubes =
        Regex.Matches(sections[1], @"\d+")
        |> Seq.map (fun value -> int value.Value)
        |> Seq.chunkBySize (cardSize * cardSize * cardSize)
        |> Seq.map (Array.ofSeq >> buildCube)
        |> Array.ofSeq

    let completedLines = cubes |> Array.map (fun _ -> Array.create lines.Length false)
    let rec callNumbers numbers bingoCount =
        match numbers with
        | number::remaining ->
            cubes
            |> Array.iter (fun cube ->
                for z in 0..cardSize - 1 do
                    for x in 0..cardSize - 1 do
                        for y in 0..cardSize - 1 do
                            if cube[z, x, y] = number then
                                cube[z, x, y] <- -1)

            let mutable newBingos = 0
            for cubeIndex, cube in Array.indexed cubes do
                let cubeCompletedLines = completedLines[cubeIndex]
                for lineIndex, line in Array.indexed lines do
                    if not cubeCompletedLines[lineIndex]
                       && line |> Array.forall (fun (z, x, y) -> cube[z, x, y] = -1) then
                        cubeCompletedLines[lineIndex] <- true
                        newBingos <- newBingos + 1

            if bingoCount + newBingos >= 5 then
                number
            else
                callNumbers remaining (bingoCount + newBingos)
        | [] -> failwith "not found 5 bingos"

    callNumbers calledNumbers 0

// Part 3
let SolvePart3 () =
    let toIndex w z x y =
        (((w * cardSize) + z) * cardSize + x) * cardSize + y

    let directions =
        [| for dw in -1..1 do
               for dz in -1..1 do
                   for dx in -1..1 do
                       for dy in -1..1 do
                           if dw > 0
                              || (dw = 0 && dz > 0)
                              || (dw = 0 && dz = 0 && dx > 0)
                              || (dw = 0 && dz = 0 && dx = 0 && dy > 0) then
                               yield dw, dz, dx, dy |]

    let lines =
        [| for dw, dz, dx, dy in directions do
               for w in 0..cardSize - 1 do
                   for z in 0..cardSize - 1 do
                       for x in 0..cardSize - 1 do
                           for y in 0..cardSize - 1 do
                               let endW = w + (cardSize - 1) * dw
                               let endZ = z + (cardSize - 1) * dz
                               let endX = x + (cardSize - 1) * dx
                               let endY = y + (cardSize - 1) * dy
                               if endW >= 0 && endW < cardSize
                                  && endZ >= 0 && endZ < cardSize
                                  && endX >= 0 && endX < cardSize
                                  && endY >= 0 && endY < cardSize then
                                   yield
                                       [| for offset in 0..cardSize - 1 ->
                                              toIndex
                                                  (w + offset * dw)
                                                  (z + offset * dz)
                                                  (x + offset * dx)
                                                  (y + offset * dy) |] |]

    let input = ReadFileAsText false 12
    let sections = Regex.Split(input.Trim(), @"\r?\n\s*\r?\n")
    let calledNumbers =
        Regex.Matches(sections[0], @"\d+")
        |> Seq.map (fun value -> int value.Value)
        |> List.ofSeq
    let hypercube =
        Regex.Matches(sections[1], @"\d+")
        |> Seq.map (fun value -> int value.Value)
        |> Array.ofSeq

    let marked = Array.create hypercube.Length false
    let completedLines = Array.create lines.Length false
    let rec callNumbers numbers bingoCount =
        match numbers with
        | number::remaining ->
            hypercube
            |> Array.iteri (fun index value ->
                if value = number then
                    marked[index] <- true)

            let mutable newBingos = 0
            for lineIndex, line in Array.indexed lines do
                if not completedLines[lineIndex]
                   && line |> Array.forall (fun index -> marked[index]) then
                    completedLines[lineIndex] <- true
                    newBingos <- newBingos + 1

            if bingoCount + newBingos >= 5 then
                number
            else
                callNumbers remaining (bingoCount + newBingos)
        | [] -> failwith "not found 5 bingos"

    callNumbers calledNumbers 0
