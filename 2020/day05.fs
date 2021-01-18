module Day05

open Expecto
open Swensen.Unquote

let splitList list indicator =
    let halfOfList = List.length list / 2
    match indicator with
    | 'L'
    | 'F' -> list |> List.take halfOfList
    | 'R'
    | 'B' -> list |> List.skip halfOfList
    | _ -> list

let getRowNumber (rowIndicator:string) =
    let rowNumbers = [ 0 .. 127 ]
    let mutable rows = rowNumbers
    for indicator in rowIndicator.ToCharArray() do
        rows <- splitList rows indicator
    
    rows.[0]

let getColumnNumber (columnIndicator:string) =
    let rowNumbers = [ 0 .. 7 ]
    let mutable rows = rowNumbers
    for indicator in columnIndicator.ToCharArray() do
        rows <- splitList rows indicator
    
    rows.[0]

let getSeatId (seatIndicator : string) =
    let rowIndicator = seatIndicator.Substring(0, 7)
    let columnIndicator = seatIndicator.Substring(7)

    let rowNumber = getRowNumber rowIndicator
    let columnNumber = getColumnNumber columnIndicator

    (rowNumber * 8) + columnNumber

[<Tests>]
let tests =
    testList "Day 05" [
        testCase "Split lists" <| fun _ ->
            test <@ splitList [0; 1] 'F' = [0] @>
            test <@ splitList [0; 1] 'B' = [1] @>
            test <@ splitList [0; 1] 'L' = [0] @>
            test <@ splitList [0; 1] 'R' = [1] @>

        testCase "Calculate row" <| fun _ ->
            test <@ getRowNumber "FBFBBFF" = 44 @>
            test <@ getColumnNumber "RLR" = 5 @>

        testCase "Get seat id" <| fun _ ->
            test <@ getSeatId "FBFBBFFRLR" = 357 @>
    ]