module AdventOfCode.Library.Day02.Part2

open AdventOfCode.Library.Day02.Common

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
    input
    |> lines
    |> Seq.filter (stringIsEmpty >> not)
    |> Seq.sumBy (fun line ->
        let (shapeString, outcomeString) = line |> words |> seqToTuple |> Option.get
        let theirShape = Option.get (stringToShape shapeString)
        let outcome = Option.get (stringToOutcome outcomeString)
        let myShape = determineShape theirShape outcome
        score myShape outcome)
    |> string
