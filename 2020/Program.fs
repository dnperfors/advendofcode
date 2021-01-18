module Program

open Expecto

let [<EntryPoint>] main argv = 
    Tests.runTestsInAssembly defaultConfig argv
