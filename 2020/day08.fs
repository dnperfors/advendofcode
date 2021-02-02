module adventofcode2020.Day08

open System
open System.Text.RegularExpressions
open Expecto
open Swensen.Unquote
open Utils

let testData = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"

type Command =
| Invalid of string
| NOP of int
| ACC of int
| JMP of int

type State = { PC : int; Acc : int; ExecutedCommands : int Set }

let parseLine (line : string) =
    let splitLine = line.Split(' ')
    let command = splitLine.[0]
    let arg = splitLine.[1] |> Int32.Parse

    match command with
    | "nop" -> NOP arg
    | "acc" -> ACC arg
    | "jmp" -> JMP arg
    | _ -> failwithf "Invalid line: %s" line

let parseContent (content : string) =
    content
    |> splitLines
    |> List.map parseLine
    |> Array.ofList

let evaluateCommand command state =
    match command with
    | NOP _ -> { state with PC = 1 + state.PC; ExecutedCommands = state.ExecutedCommands.Add state.PC }
    | ACC acc -> { state with PC = 1 + state.PC; Acc = acc + state.Acc; ExecutedCommands = state.ExecutedCommands.Add state.PC }
    | JMP jmp -> { state with PC = jmp + state.PC; ExecutedCommands = state.ExecutedCommands.Add state.PC }
    | _ -> state

let rec executeProgram (program : Command[]) state =
    if state.ExecutedCommands.Contains state.PC then
        state
    else if state.PC = Array.length program then
        state
    else
        let newState = evaluateCommand program.[state.PC] state
        executeProgram program newState

let patchCommand command =
    match command with
    | NOP number -> JMP number
    | JMP number -> NOP number
    | _ -> command

let patch (program : Command[]) fixCommandIndex =
    let command = program.[fixCommandIndex]
    let patchedCommand = patchCommand command
    let patchedProgram = Array.copy program
    Array.set patchedProgram fixCommandIndex patchedCommand
    patchedProgram

let rec fixProgram (program : Command[]) state fixCommandIndex =
    let newProgram = patch program fixCommandIndex
    let newState = executeProgram newProgram state
    if newState.PC = (Array.length program) then
        newState
    else
        fixProgram program state (fixCommandIndex + 1)

[<Tests>]
let tests =
    testList "Day 08" [
        testCase "Parse single line" <| fun _ ->
            test <@ parseLine "nop +0" = NOP 0 @>
            test <@ parseLine "acc -99" = ACC -99 @>
            test <@ parseLine "jmp +6" = JMP 6 @>

        testCase "Parse multiple lines" <| fun _ ->
            test <@ parseContent testData = [| NOP 0; ACC 1; JMP 4; ACC 3; JMP -3; ACC -99; ACC 1; JMP -4; ACC 6 |] @>

        testCase "Evaluate command" <| fun _ ->
            test <@ evaluateCommand (NOP 0) { PC = 1; Acc = 0; ExecutedCommands = Set.empty } = { PC = 2; Acc = 0; ExecutedCommands = Set.ofList([1]) } @>
            test <@ evaluateCommand (ACC 1) { PC = 1; Acc = 1; ExecutedCommands = Set.empty } = { PC = 2; Acc = 2; ExecutedCommands = Set.ofList([1]) } @>
            test <@ evaluateCommand (JMP -1) { PC = 1; Acc = 0; ExecutedCommands = Set.empty } = { PC = 0; Acc = 0; ExecutedCommands = Set.ofList([1]) } @>

        testCase "Evaluate multiple commands" <| fun _ ->
            let program = parseContent testData
            let state = { PC = 0; Acc = 0; ExecutedCommands = Set.empty }
            let state = evaluateCommand program.[state.PC] state
            test <@ state = { PC = 1; Acc = 0; ExecutedCommands = Set.ofList([0]) } @>
            let state = evaluateCommand program.[state.PC] state
            test <@ state = { PC = 2; Acc = 1; ExecutedCommands = Set.ofList([0;1]) } @>
            let state = evaluateCommand program.[state.PC] state
            test <@ state = { PC = 6; Acc = 1; ExecutedCommands = Set.ofList([0;1;2]) } @>
            let state = evaluateCommand program.[state.PC] state
            test <@ state = { PC = 7; Acc = 2; ExecutedCommands = Set.ofList([0;1;2;6]) } @>
            let state = evaluateCommand program.[state.PC] state
            test <@ state = { PC = 3; Acc = 2; ExecutedCommands = Set.ofList([0;1;2;6;7]) } @>
            let state = evaluateCommand program.[state.PC] state
            test <@ state = { PC = 4; Acc = 5; ExecutedCommands = Set.ofList([0;1;2;6;7;3]) } @>
            let state = evaluateCommand program.[state.PC] state
            test <@ state = { PC = 1; Acc = 5; ExecutedCommands = Set.ofList([0;1;2;6;7;3;4]) } @>
            let state = evaluateCommand program.[state.PC] state
            test <@ state = { PC = 2; Acc = 6; ExecutedCommands = Set.ofList([0;1;2;6;7;3;4;1]) } @>

        testList "Part 1" [
            testCase "Acceptance test" <| fun _ ->
                test <@ (executeProgram (parseContent testData) { PC = 0; Acc = 0; ExecutedCommands = Set.empty }).Acc = 5 @>

            testCase "Solution" <| fun _ ->
                test <@ (executeProgram (parseContent (getFileContent "input-day08")) { PC = 0; Acc = 0; ExecutedCommands = Set.empty }).Acc = 1727 @>
         ]

        testList "Part 2" [
            testCase "Acceptance test" <| fun _ ->
                test <@ (fixProgram (parseContent testData) { PC = 0; Acc = 0; ExecutedCommands = Set.empty } 0).Acc = 8 @>

            testCase "Solution" <| fun _ ->
                test <@ (fixProgram (parseContent (getFileContent "input-day08")) { PC = 0; Acc = 0; ExecutedCommands = Set.empty } 0).Acc = 552 @>

         ]
    ]