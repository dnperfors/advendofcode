module Day04

open System
open Utils
open Expecto
open Swensen.Unquote
open System.Text.RegularExpressions

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

let allRequiredFields = [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"; "cid" ]
let requiredFieldsForNorthPoleCredentials = [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]

let parseInput (input:string) =
    let splittedInput = input.Split(':') 
    (splittedInput.[0], splittedInput.[1])

let parsePassport (input:string) =
    input.Split([|' ';'\n';'\r'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList
    |> List.map parseInput

let splitPassportEntries (input:string) =
    input.Split([|"\n\n"; "\r\n\r\n"|], StringSplitOptions.None) |> Array.toList

let logicalAnd x y = x && y

let validateRequiredFields requiredFields passport =
    let allKeys = passport |> List.map fst
    requiredFields |> List.map (fun item -> List.contains item allKeys) |> List.fold (&&) true

let validatePassports validation input =
    input
    |> splitPassportEntries
    |> List.map (parsePassport >> validation)
    |> List.filter id
    |> List.length

let validateYear year min max = 
    year >= min && year <= max

let validateHeight height =
    let m = Regex.Match(height, "^(\d*)(in|cm)$")
    if m.Success then
        let heightValue = Int32.Parse(m.Groups.[1].Value)
        let measure = m.Groups.[2].Value
        
        match measure with
        | "cm" -> heightValue >= 150 && heightValue <= 193
        | "in" -> heightValue >= 59 && heightValue <= 76
        | _ -> false
    else
        false

let validatePassportField (name, value:string) =
    match name with
    | "byr" -> validateYear (Int32.Parse(value)) 1920 2002
    | "iyr" -> validateYear (Int32.Parse(value)) 2010 2020
    | "eyr" -> validateYear (Int32.Parse(value)) 2020 2030
    | "hgt" -> validateHeight value
    | "hcl" -> Regex.IsMatch(value, "^#[0-9a-fA-F]{6}$")
    | "ecl" -> Regex.IsMatch(value, "^(amb|blu|brn|gry|grn|hzl|oth)$")
    | "pid" -> Regex.IsMatch(value, "^\d{9}$")
    | _ -> true

let complexValidation requiredFields passport =
    if (validateRequiredFields requiredFields passport) then
        passport |> List.map validatePassportField |> List.fold (&&) true
    else
        false

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

        testCase "Split passport entries" <| fun _ ->
            test <@ splitPassportEntries "byr:1937\n\nbyr:1931" = ["byr:1937";"byr:1931"]@>

        testCase "Parse testData results in 4 passports" <| fun _ ->
            let passports = testData |> splitPassportEntries |> List.map parsePassport
            test <@ List.length passports = 4 @>
            test <@ List.length passports.[0] = 8 @>
            test <@ List.length passports.[1] = 7 @>
            test <@ List.length passports.[2] = 7 @>
            test <@ List.length passports.[3] = 6 @>

        testCase "Validate required fields" <| fun _ ->
            test <@ validateRequiredFields [ "byr"] [("byr", "1937")] @>
            test <@ not (validateRequiredFields [ "cid"] [("byr", "1937")]) @>
            test <@ [("ecl", "gry"); ("pid", "860033327"); ("eyr", "2020"); ("hcl", "#fffffd"); ("byr", "1937"); ("iyr", "2017"); ("cid", "147"); ("hgt", "183cm")] |> validateRequiredFields allRequiredFields  @>

        testList "Part1" [
            testCase "Official validation" <| fun _ ->
                test <@ validatePassports (validateRequiredFields allRequiredFields) testData = 1  @>
            testCase "North Pole Credential validation"<| fun _ ->
                test <@ validatePassports (validateRequiredFields requiredFieldsForNorthPoleCredentials) testData = 2  @>

            testCase "Solution" <| fun _ ->
                test <@ validatePassports (validateRequiredFields requiredFieldsForNorthPoleCredentials) (getFileContent "input-day04") = 202  @>
        ]

        testCase "Validate fields" <| fun _ ->
            test <@ validatePassportField ( "byr", "2002") @>
            test <@ not (validatePassportField ( "byr", "2003")) @>
            test <@ validatePassportField ( "byr", "1920") @>
            test <@ not (validatePassportField ( "byr", "1919")) @>

            test <@ validatePassportField ( "iyr", "2020") @>
            test <@ not (validatePassportField ( "iyr", "2021")) @>
            test <@ validatePassportField ( "iyr", "2010") @>
            test <@ not (validatePassportField ( "iyr", "2009")) @>

            test <@ validatePassportField ( "eyr", "2020") @>
            test <@ not (validatePassportField ( "eyr", "2019")) @>
            test <@ validatePassportField ( "eyr", "2030") @>
            test <@ not (validatePassportField ( "eyr", "2031")) @>

            test <@ not (validatePassportField ("hgt", "190" )) @>

            test <@ validatePassportField ("hgt", "59in" ) @>
            test <@ validatePassportField ("hgt", "76in" ) @>
            test <@ not (validatePassportField ("hgt", "77in" )) @>
            test <@ not (validatePassportField ("hgt", "58in" )) @>
            
            test <@ validatePassportField ("hgt", "150cm" ) @>
            test <@ validatePassportField ("hgt", "193cm" ) @>
            test <@ not (validatePassportField ("hgt", "194cm" )) @>
            test <@ not (validatePassportField ("hgt", "149cm" )) @>

            test <@ validatePassportField ( "hcl", "#123abc" ) @>
            test <@ not (validatePassportField ( "hcl", "#123abz" )) @>
            test <@ not (validatePassportField ( "hcl", "123abc" )) @>
            test <@ not (validatePassportField ( "hcl", "#abc" )) @>

            test <@ validatePassportField ( "ecl", "amb" ) @>
            test <@ validatePassportField ( "ecl", "blu" ) @>
            test <@ validatePassportField ( "ecl", "brn" ) @>
            test <@ validatePassportField ( "ecl", "gry" ) @>
            test <@ validatePassportField ( "ecl", "grn" ) @>
            test <@ validatePassportField ( "ecl", "hzl" ) @>
            test <@ validatePassportField ( "ecl", "oth" ) @>
            test <@ not (validatePassportField ( "ecl", "wat" )) @>

            test <@ validatePassportField ( "pid", "000000001" ) @>
            test <@ not (validatePassportField ( "pid", "0123456789" )) @>
            test <@ not (validatePassportField ( "pid", "abcdefghi" )) @>

            test <@ validatePassportField ( "cid", "" ) @>
            test <@ validatePassportField ( "cid", "1234567890" ) @>

        testList "Part2" [
            testCase "Acceptance tests" <| fun _ ->
                let invalidPassports = "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007"
                test <@ validatePassports (complexValidation requiredFieldsForNorthPoleCredentials) invalidPassports = 0  @>

            testCase "Acceptance test valid passports" <| fun _ ->
                let validPassports = "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
                test <@ validatePassports (complexValidation requiredFieldsForNorthPoleCredentials) validPassports = 4  @>
            
            testCase "Solution" <| fun _ ->
                test <@ validatePassports (complexValidation requiredFieldsForNorthPoleCredentials) (getFileContent "input-day04") = 137  @>
        ]
    ]