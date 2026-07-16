module LocalHelper

open System.IO

let filePath isTests day=
    let fileName = if isTests then sprintf "puzzle%02d_test.txt" day else "input.txt"
    let candidate1 = Path.Combine(Directory.GetCurrentDirectory(), sprintf "puzzle%02d" day, fileName)
    let candidate2 = Path.Combine(__SOURCE_DIRECTORY__, sprintf "puzzle%02d" day, fileName)
    if File.Exists(candidate1) then candidate1 else candidate2

let ReadFileAsText isTests day =
    let path = filePath isTests day
    File.ReadAllText(path)

let ReadFileAsLines isTests day =
    let path = filePath isTests day
    File.ReadAllLines(path)