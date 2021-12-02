namespace Advent2021

module Program =
    open System

    [<EntryPoint>]
    let main argv =

        let days = 
            [|
                {
                    Date = Day.createDate 1 12 2021
                    SolvePart1 = fun input ->
                        let lines = input.Split('\n')
                        let depths = lines |> Array.map Int32.Parse
                        let (numOfIncreases, _) = Array.fold DepthCounter.countIncrease (0, 3000) depths
                        sprintf "%i" numOfIncreases //1374
                    SolvePart2 = fun input ->
                        let lines = input.Split('\n')
                        let depths = lines |> Array.map Int32.Parse
                        let defaultState = {NumOfIncreases = 0; FirstWindow = (3000, 3000, 3000); SecondWindow = (3000, 3000, 3000);}
                        let endState = Array.fold DepthCounter.countIncreaseSlidingWindow defaultState depths
                        sprintf "%i" endState.NumOfIncreases//1418
                };
                {
                    Date = Day.createDate 2 12 2021
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
                        sprintf "%i" (horizontal * depth) //1982495697
                }
            |]
        
        

        // Run Last Day
        //days |> Array.last |> Day.runDay |> ignore
        
        // Run All Days
        days |> Array.map Day.runDay |> ignore
        0 // return an integer exit code