module AdventOfCode.Library.Day02.Part1

let stringIsEmpty (x: string) : bool = x = ""

let seqToTuple (xs: 'T seq) : 'T * 'T = (Seq.item 0 xs, Seq.item 1 xs)

type Shape =
    | Paper
    | Rock
    | Scissors

let stringToShape1 (x: string) : Shape =
    match x with
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> failwith $"invalid shape: %A{x}"

let stringToShape2 (x: string) : Shape =
    match x with
    | "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissors
    | _ -> failwith $"invalid shape: %A{x}"

type Outcome =
    | Draw
    | Lose
    | Win

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
    |> Seq.filter (stringIsEmpty >> not)
    |> Seq.sumBy (fun line ->
        let (x1, y1) = seqToTuple (line.Split " ")
        let (x2, y2) = (stringToShape1 x1, stringToShape2 y1)
        let z = play x2 y2
        score y2 z)
    |> string
