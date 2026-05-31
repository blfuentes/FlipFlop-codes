open Puzzle01

[<EntryPoint>]
let main argv =
    // Puzzle 01
    printfn "Puzzle 01 - Part 1: %d" SolvePart1
    printfn "Puzzle 01 - Part 2: %d" SolvePart2
    printfn "Puzzle 01 - Part 3: %d" SolvePart3

    // End of program
    printfn "Press any key to exit..."
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
