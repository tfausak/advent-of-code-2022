module AdventOfCode.Library.Day02.Part2

open AdventOfCode.Library.Common
open AdventOfCode.Library.Day02.Part1

let stringToOutcome (x: string) : Outcome option =
    match x with
    | "X" -> Some Lose
    | "Y" -> Some Draw
    | "Z" -> Some Win
    | _ -> None

let determineShape (shape: Shape) (outcome: Outcome) : Shape =
    match (shape, outcome) with
    | (Paper, Lose) -> Rock
    | (Paper, Win) -> Scissors
    | (Rock, Lose) -> Scissors
    | (Rock, Win) -> Paper
    | (Scissors, Lose) -> Paper
    | (Scissors, Win) -> Rock
    | (_, Draw) -> shape

let solve (input: string) : string =
    input.TrimEnd().Split "\n"
    |> Seq.sumBy (fun line ->
        let (shapeString, outcomeString) = line.Split " " |> seqToTuple |> Option.get
        let theirShape = Option.get (stringToShape shapeString)
        let outcome = Option.get (stringToOutcome outcomeString)
        let myShape = determineShape theirShape outcome
        score myShape outcome)
    |> string
