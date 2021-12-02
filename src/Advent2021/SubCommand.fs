namespace Advent2021

type SubCommand =
    | Forward of int 
    | Up of int 
    | Down of int 

module SubCommand =

    open System
    open System.Text.RegularExpressions

    let parseCommand (line: string) =
        let intString = Regex.Match(line, @"\d+").Value
        let magnitude = Int32.Parse(intString)
        if line.Contains("forward") then Forward(magnitude)
        else if line.Contains("up") then Up(magnitude)
        else if line.Contains("down") then Down(magnitude)
        else failwith "Invalid SubCommand string"

    let rec interpretCommands2d (position: int *int) (commands: SubCommand list) =
        let interpretCommand2d ((horizontal, depth): int * int) (command: SubCommand) =
            match command with
            | Forward x -> ((horizontal + x), depth)
            | Up x -> (horizontal, (depth - x))
            | Down x -> (horizontal, (depth + x))

        match commands with
        | command :: futureCommands -> interpretCommands2d (interpretCommand2d position command) futureCommands
        | [] -> position

    let rec interpretCommands (position: int * int * int) (commands: SubCommand list) =
        let interpretCommand ((horizontal, depth, aim): int * int * int) (command: SubCommand) =
            match command with
            | Forward x -> ((horizontal + x), (depth + (aim * x)), aim)
            | Up x -> (horizontal, depth, (aim - x))
            | Down x -> (horizontal, depth, (aim + x))

        match commands with
        | command :: futureCommands -> interpretCommands (interpretCommand position command) futureCommands
        | [] -> position