module Puzzle06

let calculateBirdPosition (x: int) (y: int) (vx: int) (vy: int) (time: int) (screenWidth: int) (screenHeight: int) : int * int =
    let newX = ((x + vx * time) % screenWidth + screenWidth) % screenWidth
    let newY = ((y + vy * time) % screenHeight + screenHeight) % screenHeight
    (newX, newY)

let calculateBirdPositionBig (x: int) (y: int) (vx: int) (vy: int) (time: int64) (screenWidth: int) (screenHeight: int) : int * int =
    let sw = int64 screenWidth
    let sh = int64 screenHeight
    let normalize (v:int64) (s:int64) =
        if s = 0L then 0L else ((v % s) + s) % s
    let newX64 = normalize ((int64 x) + (int64 vx) * time) sw
    let newY64 = normalize ((int64 y) + (int64 vy) * time) sh
    (int newX64, int newY64)

// Part 1
let SolvePart1 =
    let birdSpeeds = 
        LocalHelper.ReadFileAsLines false 6
        |> Seq.map (fun l -> (int(l.Split(",")[0]), int(l.Split(",")[1])))
    let (skyX, skyY, frameX, frameY) = (1000, 1000, 500, 500)
    let second = 100
    let minX = frameX / 2
    let maxX = frameX / 2 + frameX
    let minY = frameY / 2
    let maxY = frameY / 2 + frameY
    //
    let positions =
        birdSpeeds
        |> Seq.map (fun (vx, vy) -> calculateBirdPosition 0 0 vx vy second skyX skyY)
    positions
    |> Seq.filter(fun (bx, by) -> bx >= minX && bx <= maxX && by >= minY && by <= maxY)
    |> Seq.length

// Part 2
let SolvePart2 =
    let birdSpeeds = 
        LocalHelper.ReadFileAsLines false 6
        |> Seq.map (fun l -> (int(l.Split(",")[0]), int(l.Split(",")[1])))
    let (skyX, skyY, frameX, frameY) = (1000, 1000, 500, 500)
    let minX = frameX / 2
    let maxX = frameX / 2 + frameX
    let minY = frameY / 2
    let maxY = frameY / 2 + frameY

    let birdsOnHour hour =
        birdSpeeds
        |> Seq.map (fun (vx, vy) -> calculateBirdPosition 0 0 vx vy (hour * 3600) skyX skyY)
        |> Seq.filter(fun (bx, by) -> bx >= minX && bx <= maxX && by >= minY && by <= maxY)
        |> Seq.length

    [1..1000]
    |> Seq.sumBy birdsOnHour

// Part 3
let SolvePart3 =
    let birdSpeeds = 
        LocalHelper.ReadFileAsLines false 6
        |> Seq.map (fun l -> (int(l.Split(",")[0]), int(l.Split(",")[1])))
    let (skyX, skyY, frameX, frameY) = (1000, 1000, 500, 500)
    let minX = frameX / 2
    let maxX = frameX / 2 + frameX
    let minY = frameY / 2
    let maxY = frameY / 2 + frameY

    let birdsOnYear year =
        birdSpeeds
        |> Seq.map (fun (vx, vy) -> calculateBirdPositionBig 0 0 vx vy ((int64 year) * 31556926L) skyX skyY)
        |> Seq.filter(fun (bx, by) -> bx >= minX && bx <= maxX && by >= minY && by <= maxY)
        |> Seq.length

    [1..1000]
    |> Seq.sumBy birdsOnYear