module adventofcode2020.Day03

open System
open Utils
open Expecto
open Swensen.Unquote

let testData = "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"

type GroundType =
| OpenSquare
| Tree
| Invalid

let parseCharacter character =
    match character with
    | '.' -> OpenSquare
    | '#' -> Tree
    | _ -> Invalid

let parseLine (line : string) =
    line.ToCharArray()
    |> Array.toList
    |> List.map parseCharacter

let getColumn rowSize x right =
    let newColumn = x + right
    if newColumn >= rowSize then
        newColumn - rowSize
    else
        newColumn

let rec move (x, y) (right, down) (area : GroundType list list) =
    let rowSize = area.[0].Length
    let newPosition = (getColumn rowSize x right, y + down)

    if snd newPosition = area.Length - 1 then
        [ area.[snd newPosition].[fst newPosition] ]
    else
        [ area.[snd newPosition].[fst newPosition] ] @ move newPosition (right, down) area

let calculateEncounteredTrees movement area =
    area
    |> move (0,0) movement
    |> List.filter (fun item -> item = Tree)
    |> List.length
    |> uint

let runPart1 data =
    data
    |> splitLines
    |> List.map parseLine
    |> calculateEncounteredTrees (3, 1)

let multiply (x : uint) (y : uint) =
    x * y

let runPart2 data =
    let area = data |> splitLines |> List.map parseLine
    [
        calculateEncounteredTrees (1, 1) area;
        calculateEncounteredTrees (3, 1) area;
        calculateEncounteredTrees (5, 1) area;
        calculateEncounteredTrees (7, 1) area;
        calculateEncounteredTrees (1, 2) area
    ] |> List.reduce multiply

[<Tests>]
let tests =
    testList "day 03" [
        testCase "Parse line returns correct state" <| fun _ ->
            test <@ parseLine "." = [ OpenSquare ] @>
            test <@ parseLine "#" = [ Tree ] @>
            test <@ parseLine "..##" = [ OpenSquare; OpenSquare; Tree; Tree ] @>

        testCase "Move on single line returns correct GroundType" <| fun _ ->
            test <@ [ Tree ] = move (0, 0) (1, 0) [ [ OpenSquare; Tree ]] @>
            test <@ [ OpenSquare ] = move (0, 0) (1, 1) [ [ OpenSquare; Tree ]; [ Tree; OpenSquare ]] @>
            test <@ [ OpenSquare; Tree ] = move (0,0) (1,1) [ [ OpenSquare; Tree ]; [ Tree; OpenSquare ]; [Tree; OpenSquare]] @>

        testList "Part 1" [
            testCase "Acceptance test" <| fun _ ->
                test <@ 7u = runPart1 testData @>

            testCase "Solution" <| fun _ ->
                test <@ 156u = runPart1 (getFileContent "input-day03") @>
        ]

        testList "Part 2" [
            testCase "Acceptance test" <| fun _ ->
                test <@ 336u = runPart2 testData @>

            testCase "Solution" <| fun _ ->
                test <@ 3521829480u = runPart2 (getFileContent "input-day03") @>
        ]
    ]