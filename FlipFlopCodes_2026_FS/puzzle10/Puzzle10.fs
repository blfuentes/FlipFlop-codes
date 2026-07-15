module Puzzle10

open LocalHelper
open System.Text.RegularExpressions

type Op =
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
    | _ -> failwith "invalid op"

let NumOfNas (s: string) =
    Regex.Count(s, "na")

let DoOp (label: int) (op: Op) ((arg1, arg2, arg3): (int*int*Option<int>)) (registers: uint16[]) =
    let newLabel =
        match op with
        | LoadIntoReg -> 
            registers[arg2] <- (uint16)arg1
            label + 1
        | CopyFromRegToReg -> 
            registers[arg1] <- registers[arg2]
            label + 1
        | AddReg1Reg2ToReg3 -> 
            registers[arg3.Value] <- ((registers[arg1] + registers[arg2]) + uint16(65535)) % uint16(65535)
            label + 1
        | SubReg1Reg2ToReg3 -> 
            registers[arg3.Value] <- ((registers[arg1] - registers[arg2]) + uint16(65535)) % uint16(65535)
            label + 1
        | MulReg1Reg2ToReg3 -> 
            registers[arg3.Value] <- ((registers[arg1] * registers[arg2]) + uint16(65535)) % uint16(65535)
            label + 1
        | ModRegReg2ToReg3 -> 
            registers[arg3.Value] <- if registers[arg2] = uint16(0) then uint16(0) else registers[arg1] % registers[arg2]
            label + 1
        | IncValReg -> 
            registers[arg1] <- ((registers[arg1] + (uint16)1) + uint16(65535)) % uint16(65535)
            label + 1
        | DecValReg -> 
            registers[arg1] <- ((registers[arg1] - (uint16)1) + uint16(65535)) % uint16(65535)
            label + 1
        | JumpTo -> 
            arg1
        | JumpIfRegZeroTo ->
            if registers[arg1] = (uint16)0 then arg2 else label + 1
        | JumpIfRegNotZeroTo ->
            if registers[arg1] <> (uint16)0 then arg2 else label + 1
    newLabel

let countNas (parts: string array) =
    let ins = OpOfInt << NumOfNas <| parts[0].Replace("ba", "")
    let arg1 = NumOfNas <| parts[1]
    let arg2 = NumOfNas <| parts[2]
    let arg3 = if parts.Length = 4 then Some(NumOfNas <| parts[3]) else None
    (ins, arg1, arg2, arg3)

// Part 1
let SolvePart1 =
    let instructions = ReadFileAsLines true 10 |> Seq.map _.Split("ne")

    0

// Part 2
let SolvePart2 =
    0

// Part 3
let SolvePart3 =
    0