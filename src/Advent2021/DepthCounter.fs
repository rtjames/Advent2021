namespace Advent2021

type SlidingWindowState = 
    { 
        NumOfIncreases: int
        FirstWindow: int * int * int 
        SecondWindow: int * int * int
    }

module DepthCounter =

    let countIncrease ((numOfIncreases, lastDepth): int * int) (currentDepth: int) =
            if currentDepth > lastDepth then ((numOfIncreases + 1), currentDepth)
            else (numOfIncreases, currentDepth)

    let countIncreaseSlidingWindow (state: SlidingWindowState) currentDepth =
        let sumTriplet ((fst, snd, thd): int * int * int) =
            fst + snd + thd

        let (_, newFst, newSnd) = state.SecondWindow
        let newState = {state with FirstWindow = state.SecondWindow; SecondWindow = (newFst, newSnd, currentDepth)}
        if (sumTriplet newState.SecondWindow) > (sumTriplet newState.FirstWindow)
        then {newState with NumOfIncreases = newState.NumOfIncreases + 1}
        else newState