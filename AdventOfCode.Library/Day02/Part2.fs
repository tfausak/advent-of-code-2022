module AdventOfCode.Library.Day02.Part2

let seqToTuple (xs: 'T seq) : 'T * 'T = (Seq.item 0 xs, Seq.item 1 xs)

type Shape =
    | Paper
    | Rock
    | Scissors

let stringToShape (x: string) : Shape option =
    match x with
    | "A" -> Some Rock
    | "B" -> Some Paper
    | "C" -> Some Scissors
    | _ -> None

type Outcome =
    | Draw
    | Lose
    | Win

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

let shapeToScore (x: Shape) : int =
    match x with
    | Paper -> 2
    | Rock -> 1
    | Scissors -> 3

let outcomeToScore (x: Outcome) : int =
    match x with
    | Draw -> 3
    | Lose -> 0
    | Win -> 6

let score (x: Shape) (y: Outcome) : int = shapeToScore x + outcomeToScore y

let solve (input: string) : string =
    input.Split "\n"
    |> Seq.filter (fun line -> line <> "")
    |> Seq.sumBy (fun line ->
        let (shapeString, outcomeString) = seqToTuple (line.Split " ")
        let theirShape = Option.get (stringToShape shapeString)
        let outcome = Option.get (stringToOutcome outcomeString)
        let myShape = determineShape theirShape outcome
        score myShape outcome)
    |> string
