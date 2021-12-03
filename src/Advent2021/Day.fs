namespace Advent2021

open System
open System.IO

type Day =
    {
        Date: DateTime
        SolvePart1: string -> string
        SolvePart2: string -> string
    }

module Day =
    [<Literal>]
    let LIB_PATH = @"C:\_Repos\Advent2021\lib"//./../../lib/"

    let createDate day month year =
        let date = new DateTime(year, month, day)
        date.Date

    let getInputPath (day: Day) =
        let dateStr = day.Date.ToString("MM-dd-yyyy") + ".txt"
        Path.Combine(LIB_PATH, dateStr)

    let getInputForDay (day: Day) =
        File.ReadAllText(getInputPath(day))

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
            printfn "Total Time: %Os\n" (answer2Time - startTime).TotalSeconds