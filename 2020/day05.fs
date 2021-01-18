module Day05

open Expecto
open Swensen.Unquote
open Utils

let rec splitList list indicator =
    let halfOfList = List.length list / 2
    match indicator with
    | [] -> List.head list
    | head::tail ->
        let newList = match head with
                        | 'L'
                        | 'F' -> list |> List.take halfOfList
                        | 'R'
                        | 'B' -> list |> List.skip halfOfList
                        | _ -> list
        splitList newList tail

let getRowNumber (rowIndicator:string) =
    let rowNumbers = [ 0 .. 127 ]
    let rowIndicators = rowIndicator.ToCharArray() |> List.ofArray
    splitList rowNumbers rowIndicators
    
let getColumnNumber (columnIndicator:string) =
    let columnNumbers = [ 0 .. 7 ]
    let columnIndicators = columnIndicator.ToCharArray() |> List.ofArray
    splitList columnNumbers columnIndicators

let getSeatId (seatIndicator : string) =
    let rowIndicator = seatIndicator.Substring(0, 7)
    let columnIndicator = seatIndicator.Substring(7)

    let rowNumber = getRowNumber rowIndicator
    let columnNumber = getColumnNumber columnIndicator

    (rowNumber * 8) + columnNumber

let getAllSeatIds seatIndicatorList =
    seatIndicatorList
    |> splitLines
    |> List.map getSeatId

let getHighestSeatId seatIndicatorList =
    getAllSeatIds seatIndicatorList
    |> List.max

[<Tests>]
let tests =
    testList "Day 05" [
        testCase "Split lists" <| fun _ ->
            test <@ splitList [0; 1] ['F'] = 0 @>
            test <@ splitList [0; 1] ['B'] = 1 @>
            test <@ splitList [0; 1] ['L'] = 0 @>
            test <@ splitList [0; 1] ['R'] = 1 @>

        testCase "Calculate row" <| fun _ ->
            test <@ getRowNumber "FBFBBFF" = 44 @>
            test <@ getColumnNumber "RLR" = 5 @>

        testCase "Get seat id" <| fun _ ->
            test <@ getSeatId "FBFBBFFRLR" = 357 @>
            test <@ getSeatId "BFFFBBFRRR" = 567 @>
            test <@ getSeatId "FFFBBBFRRR" = 119 @>
            test <@ getSeatId "BBFFBBFRLL" = 820 @>

        testList "Part 1" [
            testCase "AcceptanceTest" <| fun _ ->
                let testData = "FBFBBFFRLR
BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL"
                test <@ getHighestSeatId testData = 820 @>

            testCase "Solution" <| fun _ ->
                test <@ getHighestSeatId (getFileContent "input-day05") = 933 @>
        ]

        testList "Part 2" [
            testCase "Solution" <| fun _ ->
                let allSeats = getAllSeatIds (getFileContent "input-day05") |> List.sort
                let possibleSeats = [ (List.min allSeats)..(List.max allSeats)]
                test <@ Set.difference (Set.ofList possibleSeats) (Set.ofList allSeats) = Set.ofList [ 711 ] @>
        ]
    ]