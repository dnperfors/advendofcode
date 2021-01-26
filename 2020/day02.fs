module adventofcode2020.Day02

open System
open System.Text.RegularExpressions
open Utils
open Expecto
open Swensen.Unquote

type Policy = { Min : int; Max : int; Character :char; Password:string}

let testData = "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc"

let parseline (input: string) =
    let m = Regex.Match(input, "(\\d*)-(\\d*) (.): (.*)")
    if m.Success then
        let lowerBound = m.Groups.[1].Value |> Int32.Parse
        let upperBound = m.Groups.[2].Value |> Int32.Parse
        let character = m.Groups.[3].Value.ToCharArray().[0]
        let password = m.Groups.[4].Value
        Some {Min = lowerBound; Max = upperBound; Character = character; Password = password}
    else
        None

let validateMinMaxCharacters policy =
    let numberOfCharacters = policy.Password.ToCharArray()
                            |> Array.toList
                            |> List.filter (fun x -> x = policy.Character)
                            |> List.length
    policy.Min <= numberOfCharacters && numberOfCharacters <= policy.Max

let validateSpecificCharacters policy =
    let firstCharacterIsValid = policy.Password.[(policy.Min - 1)] = policy.Character
    let secondCharacterIsValid = policy.Password.[(policy.Max - 1)] = policy.Character

    (firstCharacterIsValid || secondCharacterIsValid) && not (firstCharacterIsValid && secondCharacterIsValid)

let getValidPasswords filter (content : string) =
    content
    |> splitLines
    |> List.map parseline
    |> List.choose id
    |> List.filter filter

let solvePart1 data =
    data
    |> getValidPasswords validateMinMaxCharacters
    |> List.length

let solvePart2 data =
    data
    |> getValidPasswords validateSpecificCharacters
    |> List.length

[<Tests>]
let tests =
    testList "Day 02" [
        testCase "Can parse single line" <| fun _ ->
            test <@ parseline "1-3 a: abcde" = (Some {Min= 1; Max= 3; Character= 'a'; Password = "abcde"}) @>
            test <@ parseline "12-30 a: abcde" = (Some {Min= 12; Max= 30; Character= 'a'; Password = "abcde"}) @>

        testCase "Can validate policy for min and max number of characters" <| fun _ ->
            let validPolicy = { Min = 1; Max = 3; Character = 'a'; Password = "abcde" }
            let invalidPolicy = { validPolicy with Character = 'b'; Password = "cdefg" }

            test <@ validateMinMaxCharacters validPolicy @>
            test <@ not (validateMinMaxCharacters invalidPolicy) @>

        testCase "Can validate policy for character in specific positions" <| fun _ ->
            let validPolicy  = { Min = 1; Max = 3; Character = 'a'; Password = "abcde" }
            let invalidPolicy = { validPolicy with Character = 'b'; Password = "cdefg" }
            let specialPolicy = { validPolicy with Character = 'c'; Password = "ccccc" }

            test <@ validateSpecificCharacters validPolicy @>
            test <@ not (validateSpecificCharacters invalidPolicy) @>
            test <@ not (validateSpecificCharacters specialPolicy) @>

        testList "Part 1" [
            testCase "Acceptance test" <| fun _ ->
                test <@ solvePart1 testData = 2 @>

            testCase "Solution" <| fun _ ->
                test <@ solvePart1 (getFileContent "input-day02") = 546 @>
        ]

        testList "Part 2" [
            testCase "Acceptance test" <| fun _ ->
                test <@ solvePart2 testData = 1 @>

            testCase "Solution" <| fun _ ->
                test <@ solvePart2 (getFileContent "input-day02") = 275 @>
        ]
    ]