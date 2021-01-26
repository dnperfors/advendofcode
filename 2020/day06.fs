module adventofcode2020.Day06

open Expecto
open Swensen.Unquote
open Utils

let countUniqueAnswersPerGroup (input : string) =
    input.ToCharArray()
    |> Array.filter (fun item -> item >= 'a' && item <= 'z')
    |> Array.distinct
    |> Array.length

let countSharedAnswersPerGroup (input : string) =
    input
    |> splitLines
    |> List.map (fun item -> item.ToCharArray() |> Set.ofArray)
    |> List.reduce Set.intersect
    |> Set.count

let countAnswers counter (input : string) =
    input.Split("\n\n")
    |> Array.sumBy counter

[<Tests>]
let tests =
    testList "Day 06" [
        testCase "Count unique answers on single line" <| fun _ ->
            test <@ countUniqueAnswersPerGroup "a" = 1 @>
            test <@ countUniqueAnswersPerGroup "abc" = 3 @>
            test <@ countUniqueAnswersPerGroup "aabbcc" = 3 @>

        testCase "Count unique answers in a single group" <| fun _ ->
            test <@ countUniqueAnswersPerGroup "a\na" = 1 @>
            test <@ countUniqueAnswersPerGroup "a\nb\nc" = 3 @>

        testCase "Count unique answers per group" <| fun _ ->
            test <@ countAnswers countUniqueAnswersPerGroup "a\n\na" = 2 @>
            test <@ countAnswers countUniqueAnswersPerGroup "ab\n\na\nc" = 4 @>

        testCase "Count shared anwers in a single group" <| fun _ ->
            test <@ countSharedAnswersPerGroup "a" = 1 @>
            test <@ countSharedAnswersPerGroup "abc" = 3 @>
            test <@ countSharedAnswersPerGroup "aabbcc" = 3 @>
            test <@ countSharedAnswersPerGroup "a\na" = 1 @>
            test <@ countSharedAnswersPerGroup "a\nb\nc" = 0 @>

        testList "Part 1" [
            testCase "Acceptance test" <| fun _ ->
                let testData = "abc

a
b
c

ab
ac

a
a
a
a

b"
                test <@ countAnswers countUniqueAnswersPerGroup testData = 11 @>

            testCase "Solution" <| fun _ ->
                test <@ countAnswers countUniqueAnswersPerGroup (getFileContent "input-day06") = 6351 @>
        ]

        testList "Part 2" [
            testCase "Acceptance test" <| fun _ ->
                let testData = "abc

a
b
c

ab
ac

a
a
a
a

b"
                test <@ countAnswers countSharedAnswersPerGroup testData = 6 @>

            testCase "Solution" <| fun _ ->
                test <@ countAnswers countSharedAnswersPerGroup (getFileContent "input-day06") = 3143 @>
        ]
    ]