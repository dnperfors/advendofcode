module Day04

open System
open Utils
open Expecto
open Swensen.Unquote

let testData = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"

let parseInput (input:string) =
    let splittedInput = input.Split(':') 
    (splittedInput.[0], splittedInput.[1])

let parsePassport (input:string) =
    input.Split([|' ';'\n'|])
    |> Array.toList
    |> List.map parseInput

[<Tests>]
let tests =
    testList "Day 04" [
        testCase "Parse input" <| fun _ ->
            test <@ parseInput "byr:1937" = ("byr", "1937") @>
            test <@ parseInput "iyr:2017" = ("iyr", "2017") @>
            test <@ parseInput "eyr:2020" = ("eyr", "2020") @>
            test <@ parseInput "hgt:183cm" = ("hgt", "183cm") @>
            test <@ parseInput "hcl:#fffffd" = ("hcl", "#fffffd") @>
            test <@ parseInput "ecl:gry" = ("ecl", "gry") @>
            test <@ parseInput "pid:860033327" = ("pid", "860033327") @>
            test <@ parseInput "cid:147" = ("cid", "147") @>

        testCase "Parse passport entry" <| fun _ ->
            test <@ parsePassport "byr:1937 iyr:2017" = [("byr","1937"); ("iyr","2017")] @>
            test <@ parsePassport "byr:1937\niyr:2017" = [("byr","1937"); ("iyr","2017")] @>
    ]