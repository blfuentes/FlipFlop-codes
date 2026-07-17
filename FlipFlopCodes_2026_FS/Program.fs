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

    let printPuzzleDuration label action =
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
    printfn "Puzzle 03 - Part 2: %s" puzzle03Part2
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
    printfn "Puzzle 05 - Part 2: %d" puzzle05Part2
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
    printfn "Puzzle 07 - Part 3: %d" puzzle07Part3
    puzzle07Stopwatch.Stop()
    let puzzle07Elapsed = puzzle07Stopwatch.Elapsed
    printfn "Puzzle 07 takes %02d:%02d:%03d" puzzle07Elapsed.Minutes puzzle07Elapsed.Seconds puzzle07Elapsed.Milliseconds

    // Puzzle 08
    let puzzle08Stopwatch = Stopwatch.StartNew()
    let puzzle08Part1 = timeAction "Puzzle 08 part 1" (fun () -> Puzzle08.SolvePart1())
    printfn "Puzzle 08 - Part 1: %d" puzzle08Part1
    let puzzle08Part2 = timeAction "Puzzle 08 part 2" (fun () -> Puzzle08.SolvePart2())
    printfn "Puzzle 08 - Part 2: %d" puzzle08Part2
    let puzzle08Part3 = timeAction "Puzzle 08 part 3" (fun () -> Puzzle08.SolvePart3())
    printfn "Puzzle 08 - Part 3: %d" puzzle08Part3
    puzzle08Stopwatch.Stop()
    let puzzle08Elapsed = puzzle08Stopwatch.Elapsed
    printfn "Puzzle 08 takes %02d:%02d:%03d" puzzle08Elapsed.Minutes puzzle08Elapsed.Seconds puzzle08Elapsed.Milliseconds

    // Puzzle 09
    let puzzle09Stopwatch = Stopwatch.StartNew()
    let puzzle09Part1 = timeAction "Puzzle 09 part 1" (fun () -> Puzzle09.SolvePart1())
    printfn "Puzzle 09 - Part 1: %d" puzzle09Part1
    let puzzle09Part2 = timeAction "Puzzle 09 part 2" (fun () -> Puzzle09.SolvePart2())
    printfn "Puzzle 09 - Part 2: %d" puzzle09Part2
    let puzzle09Part3 = timeAction "Puzzle 09 part 3" (fun () -> Puzzle09.SolvePart3())
    printfn "Puzzle 09 - Part 3: %d" puzzle09Part3
    puzzle09Stopwatch.Stop()
    let puzzle09Elapsed = puzzle09Stopwatch.Elapsed
    printfn "Puzzle 09 takes %02d:%02d:%03d" puzzle09Elapsed.Minutes puzzle09Elapsed.Seconds puzzle09Elapsed.Milliseconds

    // Puzzle 10
    let puzzle10Stopwatch = Stopwatch.StartNew()
    let puzzle10Part1 = timeAction "Puzzle 10 part 1" (fun () -> Puzzle10.SolvePart1())
    printfn "Puzzle 10 - Part 1: %d" puzzle10Part1
    let puzzle10Part2 = timeAction "Puzzle 10 part 2" (fun () -> Puzzle10.SolvePart2())
    printfn "Puzzle 10 - Part 2: %d" puzzle10Part2
    let puzzle10Part3 = timeAction "Puzzle 10 part 3" (fun () -> Puzzle10.SolvePart3())
    printfn "Puzzle 10 - Part 3: %d" puzzle10Part3
    puzzle10Stopwatch.Stop()
    let puzzle10Elapsed = puzzle10Stopwatch.Elapsed
    printfn "Puzzle 10 takes %02d:%02d:%03d" puzzle10Elapsed.Minutes puzzle10Elapsed.Seconds puzzle10Elapsed.Milliseconds

    // Puzzle 11
    let puzzle11Stopwatch = Stopwatch.StartNew()
    let puzzle11Part1 = timeAction "Puzzle 11 part 1" (fun () -> Puzzle11.SolvePart1())
    printfn "Puzzle 11 - Part 1: %d" puzzle11Part1
    let puzzle11Part2 = timeAction "Puzzle 11 part 2" (fun () -> Puzzle11.SolvePart2())
    printfn "Puzzle 11 - Part 2: %d" puzzle11Part2
    let puzzle11Part3 = timeAction "Puzzle 11 part 3" (fun () -> Puzzle11.SolvePart3())
    printfn "Puzzle 11 - Part 3: %d" puzzle11Part3
    puzzle11Stopwatch.Stop()
    let puzzle11Elapsed = puzzle11Stopwatch.Elapsed
    printfn "Puzzle 11 takes %02d:%02d:%03d" puzzle11Elapsed.Minutes puzzle11Elapsed.Seconds puzzle11Elapsed.Milliseconds

    // Puzzle 12
    let puzzle12Stopwatch = Stopwatch.StartNew()
    let puzzle12Part1 = timeAction "Puzzle 12 part 1" (fun () -> Puzzle12.SolvePart1())
    printfn "Puzzle 12 - Part 1: %d" puzzle12Part1
    let puzzle12Part2 = timeAction "Puzzle 12 part 2" (fun () -> Puzzle12.SolvePart2())
    printfn "Puzzle 12 - Part 2: %d" puzzle12Part2
    let puzzle12Part3 = timeAction "Puzzle 12 part 3" (fun () -> Puzzle12.SolvePart3())
    printfn "Puzzle 12 - Part 3: %d" puzzle12Part3
    puzzle12Stopwatch.Stop()
    let puzzle12Elapsed = puzzle12Stopwatch.Elapsed
    printfn "Puzzle 12 takes %02d:%02d:%03d" puzzle12Elapsed.Minutes puzzle12Elapsed.Seconds puzzle12Elapsed.Milliseconds

    // End of program
    printfn "Press any key to exit..."
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code
