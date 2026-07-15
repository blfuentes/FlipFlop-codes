module Puzzle10

open LocalHelper
open System.Text.RegularExpressions
open System.Collections.Generic

type Op =
| Label
| LoadIntoReg
| CopyFromRegToReg
| AddReg1Reg2ToReg3
| SubReg1Reg2ToReg3
| MulReg1Reg2ToReg3
| ModRegReg2ToReg3
| IncValReg
| DecValReg
| JumpTo
| JumpIfRegZeroTo
| JumpIfRegNotZeroTo

let OpOfInt i =
    match i with 
    | 0  -> LoadIntoReg
    | 1  -> CopyFromRegToReg
    | 2  -> AddReg1Reg2ToReg3
    | 3  -> SubReg1Reg2ToReg3
    | 4  -> MulReg1Reg2ToReg3
    | 5  -> ModRegReg2ToReg3
    | 6  -> IncValReg
    | 7  -> DecValReg
    | 8  -> JumpTo
    | 9  -> JumpIfRegZeroTo
    | 10 -> JumpIfRegNotZeroTo
    | _  -> failwith "invalid op"

let NumOfNas (s: string) =
    Regex.Count(s, "na")

let DoOp (opIdx: int) (op: Op) ((arg1, arg2, arg3): (int*Option<int>*Option<int>)) (labels: Dictionary<int, int>) (registers: uint16[]) =
    let(a1, a2, a3) = (
        int(arg1),
        (if arg2.IsNone then -1 else int(arg2.Value)),
        (if arg3.IsNone then -1 else int(arg3.Value))
    )
    let newOpIdx =
        match op with
        | LoadIntoReg -> 
            registers[a2] <- uint16(a1)
            opIdx + 1
        | CopyFromRegToReg -> 
            registers[a2] <- registers[a1]
            opIdx + 1
        | AddReg1Reg2ToReg3 -> 
            registers[a3] <- uint16(((int(registers[a1]) + int(registers[a2])) + 65536) % 65536)
            opIdx + 1
        | SubReg1Reg2ToReg3 -> 
            registers[a3] <- uint16(((int(registers[a1]) - int(registers[a2])) + 65536) % 65536)
            opIdx + 1
        | MulReg1Reg2ToReg3 -> 
            registers[a3] <- uint16(((int(registers[a1]) * int(registers[a2])) + 65536) % 65536)
            opIdx + 1
        | ModRegReg2ToReg3 -> 
            registers[a3] <- if registers[a2] = 0us then 0us else registers[arg1] % registers[a2]
            opIdx + 1
        | IncValReg -> 
            registers[a1] <- uint16(((int(registers[a1]) + 1) + 65536) % 65536)
            opIdx + 1
        | DecValReg -> 
            registers[a1] <- uint16(((int(registers[a1]) - 1) + 65536) % 65536)
            opIdx + 1
        | JumpTo -> 
            labels[a1]
        | JumpIfRegZeroTo ->
            if registers[a1] = 0us then labels[a2] + 1 else opIdx + 1
        | JumpIfRegNotZeroTo ->
            if registers[a1] <> 0us then labels[a2] + 1 else opIdx + 1
        | Label -> labels[a1] + 1
    newOpIdx

let buildOp (parts: string array) =
    let ins = OpOfInt << NumOfNas <| parts[0].Replace("ba", "")
    let arg1 = NumOfNas <| parts[1]
    let arg2 = if parts.Length > 2 then Some(NumOfNas <| parts[2]) else None
    let arg3 = if parts.Length > 3 then Some(NumOfNas <| parts[3]) else None
    (ins, arg1, arg2, arg3)

// Part 1
let SolvePart1 =
    let instructions = ReadFileAsLines false 10 |> Seq.mapi(fun i l -> (i, l.Split("ne")))
    let labels = Dictionary<int, int>()
    let operations = 
        [
            for (line, ins) in instructions do
                if ins[0].StartsWith("be") then
                    let dir = NumOfNas <| ins[0]
                    labels.Add(dir, line)
                    yield (Label, dir, None, None)
                else
                    yield buildOp ins
        ] |> Array.ofList
    
    let mutable opIdx = 0
    let registers = Array.zeroCreate<uint16>(16)
    while opIdx < operations.Length do
        let (op, arg1, arg2, arg3) = operations[opIdx]
        opIdx <-DoOp opIdx op (arg1, arg2, arg3) labels registers
    //printfn "%s" (String.concat ", " (registers |> Seq.map string))
    registers[0]

// Part 2
let SolvePart2 =
    let instructions = ReadFileAsLines false 10 |> Seq.mapi(fun i l -> (i, l.Split("ne")))
    let labels = Dictionary<int, int>()
    let operations = 
        [
            for (line, ins) in instructions do
                if ins[0].StartsWith("be") then
                    let dir = NumOfNas <| ins[0]
                    labels.Add(dir, line)
                    yield (Label, dir, None, None)
                else
                    yield buildOp ins
        ] |> Array.ofList
    
    let mutable invalidReg0 = 0
    for r0 in [0us..99us] do
        let mutable consumedInstructions = 0
        let mutable opIdx = 0
        let registers = Array.zeroCreate<uint16>(16)
        registers[0] <- r0
        while opIdx < operations.Length && consumedInstructions <= 5000000 do
            let (op, arg1, arg2, arg3) = operations[opIdx]
            if not op.IsLabel then
                consumedInstructions <- consumedInstructions + 1
            opIdx <-DoOp opIdx op (arg1, arg2, arg3) labels registers
        //printfn "Processing with reg0= %d consumed %d instructions" r0 consumedInstructions
        if consumedInstructions > 5000000 then
            invalidReg0 <- invalidReg0 + 1

    invalidReg0

// Part 3
let SolvePart3 =
    0