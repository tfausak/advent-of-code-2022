module AdventOfCode.Library.Day02.Part1

open AdventOfCode.Library.Day02.Common

let stringToShape1 (x: string) : Shape = Option.get (stringToShape x)

let stringToShape2 (x: string) : Shape =
    match x with
    | "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissors
    | _ -> failwith $"invalid shape: %A{x}"

let play (x: Shape) (y: Shape) : Outcome =
    match (x, y) with
    | (Paper, Paper) -> Draw
    | (Paper, Rock) -> Lose
    | (Paper, Scissors) -> Win
    | (Rock, Paper) -> Win
    | (Rock, Rock) -> Draw
    | (Rock, Scissors) -> Lose
    | (Scissors, Paper) -> Lose
    | (Scissors, Rock) -> Win
    | (Scissors, Scissors) -> Draw

let solve (input: string) : string =
    input
    |> lines
    |> Seq.filter (stringIsEmpty >> not)
    |> Seq.sumBy (fun line ->
        let (x1, y1) = line |> words |> seqToTuple |> Option.get
        let (x2, y2) = (stringToShape1 x1, stringToShape2 y1)
        let z = play x2 y2
        score y2 z)
    |> string
