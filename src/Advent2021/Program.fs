namespace Advent2021

module Program =
    open System

    [<EntryPoint>]
    let main argv =

        let test = 
            """00100
            11110
            10110
            10111
            10101
            01111
            00111
            11100
            10000
            11001
            00010
            01010"""
                    //110101110100
        let test2 = //001010001011 651 3444//13820
            """000001100010
            100111011010
            001100011001
            011010001010
            011010101011
            001001110101
            100110001101"""

        let parseBinary (binStr: string) =
            let s = binStr.Trim()
            Convert.ToUInt16(s, 2)

        let countOnes (bits: uint16 list) =
            let incrementIfOdd (count: int) (num: uint16) =
                if not (num % 2us = 0us) then (count + 1) 
                else count

            bits 
            |> List.fold incrementIfOdd 0

        let countOnesAtPos (pos: int) (bits: uint16 list) =
            bits |> List.map (fun bit -> bit >>> pos) |> List.fold (fun numOfOnes bit -> if bit % 2us = 0us then numOfOnes + 1 else numOfOnes) 0

        let calcGammaRateStr bits =
            let rec loop (count: int) (onesList: int list) (bits: uint16 list) =
                if count > 4 then onesList
                else
                    loop (count + 1) ((countOnes bits) :: onesList) (bits |> List.map (fun bit -> bit >>> 1))

            let gammaList = loop 0 [] bits |> List.map (fun num -> if num > (bits.Length / 2) then 1 else 0)
            gammaList |> List.map (fun num -> num .ToString()) |> String.Concat

        let calcEpsilonRateStr (gammaRateStr: string) = 
            let invertBin (binChar: char) =
                match binChar with 
                | '0' -> '1'
                | '1' -> '0'
                | _ -> failwith "Invalid Binary Character"

            gammaRateStr |> Seq.map invertBin |> String.Concat

        let rec filterBitsByStr (str: string) (pos: int) (bits: string list) =
            if bits.Length = 1 then bits.Head
            else
                let newBits = List.filter (fun (bit: string) -> bit.[pos] = str.[pos]) bits
                filterBitsByStr str (pos + 1) newBits

        let rec filterBits (pos: int) (bits: uint16 list) =
            if bits.Length = 1 then bits.Head
            else
                let reversePos = 11 - pos
                let numOfOnes = countOnesAtPos reversePos bits
                let bitFilter = if numOfOnes >= (bits.Length / 2) then 1 else 0
                let newBits = List.filter (fun (bit: string) -> bit.[pos] = bitFilter) bits
                filterBitsByStr str (pos + 1) newBits

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
                        let bits = lines |> Array.map parseBinary |> List.ofArray
                        let gammaRateStr = calcGammaRateStr bits
                        let epsRateStr = calcEpsilonRateStr gammaRateStr
                        let gamma = parseBinary gammaRateStr
                        let epsilon = parseBinary epsRateStr
                        let powerConsumption = ((int)gamma * (int)epsilon)
                        sprintf "%i" powerConsumption //3958484
                    SolvePart2 = fun input ->
                        let lines = test.Split('\n') |> Array.map (fun s -> s.Trim())
                        let bits = lines |> Array.map parseBinary |> List.ofArray
                        let gammaRateStr = calcGammaRateStr bits
                        let epsRateStr = calcEpsilonRateStr gammaRateStr
                        let bitStrs = lines |> List.ofArray
                        let oxyGenStr = filterBitsByStr gammaRateStr 0 bitStrs
                        let co2ScrubStr = filterBitsByStr epsRateStr 0 bitStrs
                        let oxyGen = parseBinary oxyGenStr
                        let co2Scrub = parseBinary co2ScrubStr
                        let powerConsumption = ((int)oxyGen * (int)co2Scrub)
                        sprintf "%i" powerConsumption //3950891 too high
                }
            |]
        
        

        // Run Last Day
        days |> Array.last |> Day.runDay |> ignore
        
        // Run All Days
        //days |> Array.map Day.runDay |> ignore
        0 // return an integer exit code