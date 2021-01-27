module adventofcode2020.Day07

open System
open System.Text.RegularExpressions
open Expecto
open Swensen.Unquote
open Utils

let testData = "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."

let testData2 = "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags."

let parseLineColor line =
    let m = Regex("^(.*?) bags").Match(line)
    m.Groups.[1].Value

let extractContent line =
    Regex("(\d) (.*?) bag").Matches(line)
    |> Seq.map (fun m -> (m.Groups.[2].Value, (Int32.Parse m.Groups.[1].Value)))
    |> Seq.toList

let parseLine line =
    let lineColor = parseLineColor line
    let content = extractContent line
    (lineColor, content)

let parseLines content =
    splitLines content
    |> List.map parseLine

let reverseRules rules =
    [
        for (container, content) in rules do
            for (color, _) in content do
                (color, container)
    ]

let rec getContainersWithColor color reversedRules =
    let containersWithDirectColor = 
        reversedRules
        |> List.filter (fun (child, _) -> child = color)

    containersWithDirectColor @ (containersWithDirectColor |> List.collect (fun (_, parent) -> getContainersWithColor parent reversedRules) )

let countCanContain color rules =
    getContainersWithColor color (reverseRules rules)
   |> List.map (fun (_, bag) -> bag)
   |> List.distinct
   |> List.length

let rec countNumberOfBags (color:string) (rules :(string * (string * int) list) list) =
    Map.ofList rules
    |> Map.find color
    |> List.map (fun (child, amount) -> amount + (amount * countNumberOfBags child rules))
    |> List.sum
    
[<Tests>]
let test =
    testList "Day 07" [
        testCase "Parse single line will return tuple" <| fun _ ->
            test <@ parseLine "navy blue bags contain no other bags." = ("navy blue", []) @>
            test <@ parseLine "blue bags contain 1 red bag." = ("blue", [("red", 1)]) @>
            test <@ parseLine "blue bags contain 2 red bags." = ("blue", [("red", 2)]) @>
            test <@ parseLine "blue bags contain 2 red bags, 1 green bag." = ("blue", [("red", 2); ("green", 1)]) @>
            test <@ parseLine "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags." = ("vibrant plum", [("faded blue", 5); ("dotted black", 6)]) @>

        testCase "Parse test data will return all correct tuples" <| fun _ ->
            let expected = [
                ("light red", [("bright white", 1); ("muted yellow", 2)])
                ("dark orange", [("bright white", 3); ("muted yellow", 4)])
                ("bright white", [("shiny gold", 1)])
                ("muted yellow", [("shiny gold", 2); ("faded blue", 9)])
                ("shiny gold", [("dark olive", 1); ("vibrant plum", 2)])
                ("dark olive", [("faded blue", 3); ("dotted black", 4)])
                ("vibrant plum", [("faded blue", 5); ("dotted black", 6)])
                ("faded blue", [])
                ("dotted black", [])
            ] 
            test <@ parseLines testData = expected @>

        testCase "Reverse rules" <| fun _ ->
            let expected = [
                ("bright white", "light red")
                ("muted yellow", "light red")
                ("bright white", "dark orange")
                ("muted yellow", "dark orange")
                ("shiny gold", "bright white")
                ("shiny gold", "muted yellow")
                ("faded blue", "muted yellow")
                ("dark olive", "shiny gold")
                ("vibrant plum", "shiny gold")
                ("faded blue", "dark olive")
                ("dotted black", "dark olive")
                ("faded blue", "vibrant plum")
                ("dotted black", "vibrant plum")
            ]
            test <@ reverseRules (parseLines testData) = expected @>

        testList "Part 1" [
            testCase "Acceptance test" <| fun _ ->
                test <@ parseLines testData |> countCanContain "shiny gold" = 4 @>

            testCase "Solution" <| fun _ ->
                test <@ parseLines (getFileContent "input-day07") |> countCanContain "shiny gold" = 274 @>
        ]
        
        testList "Part 2" [
            testCase "Acceptance test" <| fun _ ->
                test <@ parseLines testData |> countNumberOfBags "faded blue" = 0 @>
                test <@ parseLines testData |> countNumberOfBags "vibrant plum" = 11 @>
                test <@ parseLines testData |> countNumberOfBags "dark olive" = 7 @>
                test <@ parseLines testData |> countNumberOfBags "shiny gold" = 32 @>
                test <@ parseLines testData2 |> countNumberOfBags "shiny gold" = 126 @>
                
            testCase "Solution" <| fun _ ->
                test <@ parseLines (getFileContent "input-day07") |> countNumberOfBags "shiny gold" = 158730 @>
        ]
    ]
