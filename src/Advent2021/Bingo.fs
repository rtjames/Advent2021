namespace Advent2021

type BingoCell =
    {
        Number: int
        IsMarked: bool
    }


module Bingo =

    let createCell (num: int) =
        { Number = num; IsMarked = false }

    let markCell (cell: BingoCell) =
        { cell with IsMarked = true; }

    let createBoard (nums: int [,]) =
        let initialize i j =
            nums.[i,j] |> createCell
        Array2D.init 5 5 initialize

    let markBoard (num: int) (board: BingoCell [,]) =
        let markIfNum (cell: BingoCell) =
            if cell.Number = num then (markCell cell)
            else cell
        Array2D.map markIfNum board

    let hasWon (board: BingoCell [,]) =
        let checkRowColum (index: int) =
            let row = board.[index, *]
            let rowWins = Array.forall (fun cell -> cell.IsMarked) row
            if rowWins then true
            else
                let column = board.[*, index]
                Array.forall (fun cell -> cell.IsMarked) column
        let checkDiagonal (i: int) =
            if board.[i,i].IsMarked then checkRowColum i 
            else false
        seq {0..4} |> Seq.exists checkDiagonal