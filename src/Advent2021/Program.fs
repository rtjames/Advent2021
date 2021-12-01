namespace Advent2021

open System
open System.IO

type SlidingWindowState = 
    { 
        NumOfIncreases: int
        FirstWindow: int * int * int 
        SecondWindow: int * int * int
    }

module Program =

    [<EntryPoint>]
    let main argv =
        let message = 
            let date = new DateTime(2021, 12, 1)
            date.Date.ToString("MM-dd-yyyy")

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

        
        
            

        let input = File.ReadAllText("./../../lib/12-01-2021.txt")
        let lines = input.Split('\n')
        let depths = lines |> Array.map Int32.Parse
        //let (numOfIncreases, _) = Array.fold countIncrease (0, 3000) depths //1374
        //printfn "%i" numOfIncreases //1374
        let defaultState = {NumOfIncreases = 0; FirstWindow = (3000, 3000, 3000); SecondWindow = (3000, 3000, 3000);}
        let endState = Array.fold countIncreaseSlidingWindow defaultState depths
        printfn "%i" endState.NumOfIncreases//1418
        0 // return an integer exit code