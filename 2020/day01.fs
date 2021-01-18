module Day01

open System
open Utils
open Expecto
open Swensen.Unquote

let testData = "1721\r\n979\r\n366\r\n299\r\n675\r\n1456"

let readNumbers (content:string) =
    content
    |> splitLines
    |> List.map Int32.Parse

let rec comb n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

let sumsTo2020 itemList =
    if 2020 = (List.sum itemList) then
        Some itemList
    else
        None

let multiply accumulator item =
    accumulator * item

let getPairOf2Summing2020 numbers =
    numbers
    |> comb 2
    |> List.pick sumsTo2020

let getPairOf3Summing2020 numbers =
    numbers
    |> comb 3
    |> List.pick sumsTo2020

let multiplyPairs numberlist =
    numberlist
    |> List.reduce multiply

let solveDay1 content =
    content
    |> readNumbers
    |> getPairOf2Summing2020
    |> multiplyPairs

let solveDay1Part2 content =
    content
    |> readNumbers
    |> getPairOf3Summing2020
    |> multiplyPairs

[<Tests>]
let test =
    testList "Day 01" [
        testCase "get pairs" <| fun _ ->
            test <@ comb 2 [1;2;3] = [[1;2];[1;3];[2;3]] @>
            test <@ comb 3 [1;2;3] = [[1;2;3]] @>

        testCase "find specific pair" <| fun _ ->
            let data = [1721; 979; 366; 299; 675; 1459]
            test <@ getPairOf2Summing2020 data = [1721; 299] @>
            test <@ getPairOf3Summing2020 data = [979; 366; 675] @>
        
        testList "Part 1" [
            testCase "Acceptance test" <| fun _ ->    
                test <@ solveDay1 testData = 514579 @>

            testCase "Solution" <| fun _ ->
                test <@ solveDay1 (getFileContent "input-day01") = 485739 @>
        ]

        testList "Part 2" [
            testCase "Acceptance test" <| fun _ ->    
                test <@ solveDay1Part2 testData = 241861950 @>

            testCase "Solution" <| fun _ ->
                test <@ solveDay1Part2 (getFileContent "input-day01") = 161109702 @>
        ]
    ]