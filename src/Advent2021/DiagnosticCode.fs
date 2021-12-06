namespace Advent2021

type DiagnosticCode =
    {
        Value: uint16
        NumOfBits: int
    }

module DiagnosticCode =
    open System

    let Create (bitStr: string) =
        let str = bitStr.Trim()
        { Value = Convert.ToUInt16(str, 2); NumOfBits = str.Length }

    let getBitAtPosition (position: int) (code: DiagnosticCode)  =
        let bitIsZero = (code.Value >>> (code.NumOfBits - position)) % 2us = 0us
        if bitIsZero then 0 else 1

    let GetMostCommonBitAtPosition (codes: DiagnosticCode list) (position: int) =
        let sumOfBits = List.map (getBitAtPosition position) codes |> List.sum |> float
        let half = (float)codes.Length / 2.0
        match sumOfBits with 
        | x when x < half -> 0
        | _ -> 1

    let GetLeastCommonBitAtPosition (codes: DiagnosticCode list) (position: int) =
        let mostCommonBit = GetMostCommonBitAtPosition codes position
        if mostCommonBit = 1 then 0 else 1

    let CalculateGammaRate (codes: DiagnosticCode list) = 
        let gammaSeq = 
            seq {
                for position in 1..codes.Head.NumOfBits do
                    GetMostCommonBitAtPosition codes position
            }

        Seq.map (fun bit -> bit.ToString()) gammaSeq |> String.Concat |> Create

    let Invert (code: DiagnosticCode) =
        let invertBin (binChar: char) =
            match binChar with 
            | '0' -> '1'
            | '1' -> '0'
            | _ -> failwith "Invalid Binary Character"

        let bitStr = Convert.ToString((int)code.Value, 2)
        bitStr.[(bitStr.Length - code.NumOfBits)..(bitStr.Length - 1)]
        |> Seq.map invertBin
        |> String.Concat
        |> Create

    let CalculatePowerConsumption (codes: DiagnosticCode list) =
        let gammaRate = CalculateGammaRate codes
        let epsilonRate = Invert gammaRate
        (int)gammaRate.Value * (int)epsilonRate.Value

    let filterBy (filter: int) (codes: DiagnosticCode list) (position: int) =
        if codes.Length = 1 then codes 
        else 
            List.filter (fun code -> (getBitAtPosition position code) = filter) codes

    let CalculateOxyGenRating (codes: DiagnosticCode list) =
        let filterByMostCommonBit codesToFilter position = 
            let filterBit = GetMostCommonBitAtPosition codesToFilter position
            filterBy filterBit codesToFilter position

        seq { 1 .. codes.Head.NumOfBits}
        |> Seq.fold filterByMostCommonBit codes
        |> List.head

    let CalculateCo2ScrubberRating (codes: DiagnosticCode list) =
        let filterByLeastCommonBit codes position = filterBy (GetLeastCommonBitAtPosition codes position) codes position
        seq { 1 .. codes.Head.NumOfBits}
        |> Seq.fold filterByLeastCommonBit codes
        |> List.head

    let CalculateLifeSupportRating (codes: DiagnosticCode list) =
        let oxyGenRating = CalculateOxyGenRating codes
        let co2ScrubberRating = CalculateCo2ScrubberRating codes
        (int)oxyGenRating.Value * (int)co2ScrubberRating.Value
