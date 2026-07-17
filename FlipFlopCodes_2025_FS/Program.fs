open System.Diagnostics

[<EntryPoint>]
let main argv =
    let timeAction label action =
        let sw = Stopwatch.StartNew()
        let result = action ()
        sw.Stop()
        let elapsed = sw.Elapsed
        printfn "%s takes %02d:%02d:%03d" label elapsed.Minutes elapsed.Seconds elapsed.Milliseconds
        result

    // Puzzle 01
    let puzzle01Stopwatch = Stopwatch.StartNew()
    let puzzle01Part1 = timeAction "Puzzle 01 part 1" (fun () -> Puzzle01.SolvePart1())
    printfn "Puzzle 01 - Part 1: %d" puzzle01Part1
    let puzzle01Part2 = timeAction "Puzzle 01 part 2" (fun () -> Puzzle01.SolvePart2())
    printfn "Puzzle 01 - Part 2: %d" puzzle01Part2
    let puzzle01Part3 = timeAction "Puzzle 01 part 3" (fun () -> Puzzle01.SolvePart3())
    printfn "Puzzle 01 - Part 3: %d" puzzle01Part3
    puzzle01Stopwatch.Stop()
    let puzzle01Elapsed = puzzle01Stopwatch.Elapsed
    printfn "Puzzle 01 takes %02d:%02d:%03d" puzzle01Elapsed.Minutes puzzle01Elapsed.Seconds puzzle01Elapsed.Milliseconds

    // Puzzle 02
    let puzzle02Stopwatch = Stopwatch.StartNew()
    let puzzle02Part1 = timeAction "Puzzle 02 part 1" (fun () -> Puzzle02.SolvePart1())
    printfn "Puzzle 02 - Part 1: %d" puzzle02Part1
    let puzzle02Part2 = timeAction "Puzzle 02 part 2" (fun () -> Puzzle02.SolvePart2())
    printfn "Puzzle 02 - Part 2: %d" puzzle02Part2
    let puzzle02Part3 = timeAction "Puzzle 02 part 3" (fun () -> Puzzle02.SolvePart3())
    printfn "Puzzle 02 - Part 3: %d" puzzle02Part3
    puzzle02Stopwatch.Stop()
    let puzzle02Elapsed = puzzle02Stopwatch.Elapsed
    printfn "Puzzle 02 takes %02d:%02d:%03d" puzzle02Elapsed.Minutes puzzle02Elapsed.Seconds puzzle02Elapsed.Milliseconds

    // Puzzle 03
    let puzzle03Stopwatch = Stopwatch.StartNew()
    let puzzle03Part1 = timeAction "Puzzle 03 part 1" (fun () -> Puzzle03.SolvePart1())
    printfn "Puzzle 03 - Part 1: %s" puzzle03Part1
    let puzzle03Part2 = timeAction "Puzzle 03 part 2" (fun () -> Puzzle03.SolvePart2())
    printfn "Puzzle 03 - Part 2: %d" puzzle03Part2
    let puzzle03Part3 = timeAction "Puzzle 03 part 3" (fun () -> Puzzle03.SolvePart3())
    printfn "Puzzle 03 - Part 3: %d" puzzle03Part3
    puzzle03Stopwatch.Stop()
    let puzzle03Elapsed = puzzle03Stopwatch.Elapsed
    printfn "Puzzle 03 takes %02d:%02d:%03d" puzzle03Elapsed.Minutes puzzle03Elapsed.Seconds puzzle03Elapsed.Milliseconds

    // Puzzle 04
    let puzzle04Stopwatch = Stopwatch.StartNew()
    let puzzle04Part1 = timeAction "Puzzle 04 part 1" (fun () -> Puzzle04.SolvePart1())
    printfn "Puzzle 04 - Part 1: %d" puzzle04Part1
    let puzzle04Part2 = timeAction "Puzzle 04 part 2" (fun () -> Puzzle04.SolvePart2())
    printfn "Puzzle 04 - Part 2: %d" puzzle04Part2
    let puzzle04Part3 = timeAction "Puzzle 04 part 3" (fun () -> Puzzle04.SolvePart3())
    printfn "Puzzle 04 - Part 3: %d" puzzle04Part3
    puzzle04Stopwatch.Stop()
    let puzzle04Elapsed = puzzle04Stopwatch.Elapsed
    printfn "Puzzle 04 takes %02d:%02d:%03d" puzzle04Elapsed.Minutes puzzle04Elapsed.Seconds puzzle04Elapsed.Milliseconds

    // Puzzle 05
    let puzzle05Stopwatch = Stopwatch.StartNew()
    let puzzle05Part1 = timeAction "Puzzle 05 part 1" (fun () -> Puzzle05.SolvePart1())
    printfn "Puzzle 05 - Part 1: %d" puzzle05Part1
    let puzzle05Part2 = timeAction "Puzzle 05 part 2" (fun () -> Puzzle05.SolvePart2())
    printfn "Puzzle 05 - Part 2: %s" puzzle05Part2
    let puzzle05Part3 = timeAction "Puzzle 05 part 3" (fun () -> Puzzle05.SolvePart3())
    printfn "Puzzle 05 - Part 3: %d" puzzle05Part3
    puzzle05Stopwatch.Stop()
    let puzzle05Elapsed = puzzle05Stopwatch.Elapsed
    printfn "Puzzle 05 takes %02d:%02d:%03d" puzzle05Elapsed.Minutes puzzle05Elapsed.Seconds puzzle05Elapsed.Milliseconds

    // Puzzle 06
    let puzzle06Stopwatch = Stopwatch.StartNew()
    let puzzle06Part1 = timeAction "Puzzle 06 part 1" (fun () -> Puzzle06.SolvePart1())
    printfn "Puzzle 06 - Part 1: %d" puzzle06Part1
    let puzzle06Part2 = timeAction "Puzzle 06 part 2" (fun () -> Puzzle06.SolvePart2())
    printfn "Puzzle 06 - Part 2: %d" puzzle06Part2
    let puzzle06Part3 = timeAction "Puzzle 06 part 3" (fun () -> Puzzle06.SolvePart3())
    printfn "Puzzle 06 - Part 3: %d" puzzle06Part3
    puzzle06Stopwatch.Stop()
    let puzzle06Elapsed = puzzle06Stopwatch.Elapsed
    printfn "Puzzle 06 takes %02d:%02d:%03d" puzzle06Elapsed.Minutes puzzle06Elapsed.Seconds puzzle06Elapsed.Milliseconds

    // Puzzle 07
    let puzzle07Stopwatch = Stopwatch.StartNew()
    let puzzle07Part1 = timeAction "Puzzle 07 part 1" (fun () -> Puzzle07.SolvePart1())
    printfn "Puzzle 07 - Part 1: %d" puzzle07Part1
    let puzzle07Part2 = timeAction "Puzzle 07 part 2" (fun () -> Puzzle07.SolvePart2())
    printfn "Puzzle 07 - Part 2: %d" puzzle07Part2
    let puzzle07Part3 = timeAction "Puzzle 07 part 3" (fun () -> Puzzle07.SolvePart3())
    printfn "Puzzle 07 - Part 3: %A" puzzle07Part3
    puzzle07Stopwatch.Stop()
    let puzzle07Elapsed = puzzle07Stopwatch.Elapsed
    printfn "Puzzle 07 takes %02d:%02d:%03d" puzzle07Elapsed.Minutes puzzle07Elapsed.Seconds puzzle07Elapsed.Milliseconds

    // End of program
    printfn "Press any key to exit..."
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
