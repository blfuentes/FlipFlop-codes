open Puzzle01

[<EntryPoint>]
let main argv =
    // Puzzle 01
    printfn "Puzzle 01 - Part 1: %d" SolvePart1
    printfn "Puzzle 01 - Part 2: %d" SolvePart2
    printfn "Puzzle 01 - Part 3: %d" SolvePart3


    // Puzzle 02
    printfn "Puzzle 02 - Part 1: %d" Puzzle02.SolvePart1
    printfn "Puzzle 02 - Part 2: %d" Puzzle02.SolvePart2
    printfn "Puzzle 02 - Part 3: %d" Puzzle02.SolvePart3

    // Puzzle 03
    printfn "Puzzle 03 - Part 1: %s" Puzzle03.SolvePart1
    printfn "Puzzle 03 - Part 2: %d" Puzzle03.SolvePart2
    printfn "Puzzle 03 - Part 3: %d" Puzzle03.SolvePart3

    // End of program
    printfn "Press any key to exit..."
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
