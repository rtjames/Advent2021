namespace Advent2021

open System
open System.IO

type Day =
    {
        Date: DateTime
        SolvePart1: string -> string
        SolvePart2: string -> string
    }

type SlidingWindowState = 
    { 
        NumOfIncreases: int
        FirstWindow: int * int * int 
        SecondWindow: int * int * int
    }

module Program =
    [<Literal>]
    let LIB_PATH = "./../../lib/"

    let createDate day month year =
        let date = new DateTime(year, month, day)
        date.Date

    let getInputPath (day: Day) =
        let dateStr = day.Date.ToString("MM-dd-yyyy") + ".txt"
        Path.Combine(LIB_PATH, dateStr)

    let countIncrease ((numOfIncreases, lastDepth): int * int) (currentDepth: int) =
            if currentDepth > lastDepth then ((numOfIncreases + 1), currentDepth)
            else (numOfIncreases, currentDepth)

    let sumTriplet ((fst, snd, thd): int * int * int) =
        fst + snd + thd

    let countIncreaseSlidingWindow (state: SlidingWindowState) currentDepth =
        let (_, newFst, newSnd) = state.SecondWindow
        let newState = {state with FirstWindow = state.SecondWindow; SecondWindow = (newFst, newSnd, currentDepth)}
        if (sumTriplet newState.SecondWindow) > (sumTriplet newState.FirstWindow)
        then {newState with NumOfIncreases = newState.NumOfIncreases + 1}
        else newState

    let getInputForDay (day: Day) =
        File.ReadAllText(getInputPath(day))

    [<EntryPoint>]
    let main argv =
        
        let day1Part1 (input: string) =
            let lines = input.Split('\n')
            let depths = lines |> Array.map Int32.Parse
            let (numOfIncreases, _) = Array.fold countIncrease (0, 3000) depths //1374
            sprintf "%i" numOfIncreases //1374

        let day1Part2 (input: string) =
            let lines = input.Split('\n')
            let depths = lines |> Array.map Int32.Parse
            let defaultState = {NumOfIncreases = 0; FirstWindow = (3000, 3000, 3000); SecondWindow = (3000, 3000, 3000);}
            let endState = Array.fold countIncreaseSlidingWindow defaultState depths
            sprintf "%i" endState.NumOfIncreases//1418

        let days = 
            [|
                {
                    Date = createDate 1 12 2021
                    SolvePart1 = fun input ->
                        let lines = input.Split('\n')
                        let depths = lines |> Array.map Int32.Parse
                        let (numOfIncreases, _) = Array.fold countIncrease (0, 3000) depths
                        sprintf "%i" numOfIncreases //1374
                    SolvePart2 = fun input ->
                        let lines = input.Split('\n')
                        let depths = lines |> Array.map Int32.Parse
                        let defaultState = {NumOfIncreases = 0; FirstWindow = (3000, 3000, 3000); SecondWindow = (3000, 3000, 3000);}
                        let endState = Array.fold countIncreaseSlidingWindow defaultState depths
                        sprintf "%i" endState.NumOfIncreases//1418
                };
                {
                    Date = createDate 2 12 2021
                    SolvePart1 = fun input ->
                        let startPos = (0, 0)
                        let lines = input.Split('\n')
                        let (horizontal, depth) = 
                            lines 
                            |> List.ofArray 
                            |> List.map SubCommand.parseCommand 
                            |> SubCommand.interpretCommands2d startPos
                        sprintf "%i" (horizontal * depth) //1924923
                    SolvePart2 = fun input ->
                        let startPos = (0, 0, 0)
                        let lines = input.Split('\n')
                        let (horizontal, depth, _) = 
                            lines 
                            |> List.ofArray 
                            |> List.map SubCommand.parseCommand 
                            |> SubCommand.interpretCommands startPos
                        sprintf "%i" (horizontal * depth) 
                }
            |]
        
        let lastDay = Array.sortBy (fun day -> day.Date) days |> Array.last

        let runDay (day: Day) =
            let startTime = DateTime.Now
            let input = getInputForDay day
            let ioTime = DateTime.Now
            let answer1 = day.SolvePart1 input
            let answer1Time = DateTime.Now
            let answer2 = day.SolvePart2 input
            let answer2Time = DateTime.Now
            printfn "%s" (day.Date.ToString("MM-dd-yyyy"))
            printfn "Part 1\n------\n%s\n%Os\n" answer1 (answer1Time - ioTime).TotalSeconds
            printfn "Part 2\n------\n%s\n%Os\n" answer2 (answer2Time - answer1Time).TotalSeconds
            printfn "IO Time: %Os" (ioTime - startTime).TotalSeconds
            printfn "Total Time: %Os" (answer2Time - startTime).TotalSeconds

        runDay lastDay
        0 // return an integer exit code