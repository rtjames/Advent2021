namespace Advent2021

module Program =
    open System

    [<EntryPoint>]
    let main argv =
        let test = """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
        
        22 13 17 11  0
         8  2 23  4 24
        21  9 14 16  7
         6 10  3 18  5
         1 12 20 15 19
        
         3 15  0  2 22
         9 18 13 17  5
        19  8  7 25 23
        20 11 10 24  4
        14 21 16 12  6
        
        14 21 17 24  4
        10 16 15  9 19
        18  8 23 26 20
        22 11 13  6  5
         2  0 12  3  7"""

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
                };
                {
                    Date = Day.createDate 3 12 2021
                    SolvePart1 = fun input ->
                        let lines = input.Split('\n')
                        let codes = lines |> Array.toList |> List.map DiagnosticCode.Create
                        let powerConsumption = DiagnosticCode.CalculatePowerConsumption codes
                        sprintf "%i" powerConsumption //3958484
                    SolvePart2 = fun input ->
                        let lines = input.Split('\n')
                        let codes = lines |> Array.toList |> List.map DiagnosticCode.Create
                        let lifeSupportRating = DiagnosticCode.CalculateLifeSupportRating codes
                        sprintf "%i" lifeSupportRating //1613181
                };
                {
                    Date = Day.createDate 4 12 2021
                    SolvePart1 = fun input ->
                        let lines = input.Split("\n\r\n")
                        let numbers = lines.[0].Split(',') |> Array.map Int32.Parse |> List.ofArray
                        let create2dArrayFromBoardStr (boardStr: string) =
                            let s = boardStr.Trim()
                            let sSplit = s.Split("\r\n") 
                            sSplit 
                            |> Array.map (fun str -> str.Split(' ') |> Array.filter (fun x -> not (x = ""))) 
                            |> array2D 
                            |> Array2D.map Int32.Parse 
                        let boards = 
                            lines.[1..] 
                            |> List.ofArray 
                            |> List.map create2dArrayFromBoardStr 
                            |> List.map Bingo.createBoard
                        let findWinner =
                                let rec checkBoards (nums: int list) (boards: BingoCell[,] list) =
                                    let markedBoards = List.map (Bingo.markBoard nums.Head) boards
                                    match List.tryFind Bingo.hasWon markedBoards with
                                    | Some b -> (nums.Head, b)
                                    | _ -> checkBoards nums.Tail markedBoards

                                checkBoards numbers boards
                        findWinner |> ignore
                        ""
                    SolvePart2 = fun input ->
                        ""
                }
            |]
        
        

        // Run Last Day
        days |> Array.last |> Day.runDay |> ignore
        
        // Run All Days
        //days |> Array.map Day.runDay |> ignore
        0 // return an integer exit code