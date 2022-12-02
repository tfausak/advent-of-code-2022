module AdventOfCode.Library.Day02.Common

let lines (x: string) : string[] = x.Split "\n"

let seqToTuple (xs: 'T seq) : ('T * 'T) option =
    match Seq.tryItem 2 xs with
    | Some _ -> None
    | None -> Option.map2 (fun x y -> (x, y)) (Seq.tryItem 0 xs) (Seq.tryItem 1 xs)

let stringIsEmpty (x: string) : bool = x = ""

let words (x: string) : string[] = x.Split " "

type Outcome =
    | Draw
    | Lose
    | Win

let outcomeToScore (x: Outcome) : int =
    match x with
    | Draw -> 3
    | Lose -> 0
    | Win -> 6

let stringToOutcome (x: string) : Outcome option =
    match x with
    | "X" -> Some Lose
    | "Y" -> Some Draw
    | "Z" -> Some Win
    | _ -> None

type Shape =
    | Paper
    | Rock
    | Scissors

let shapeToScore (x: Shape) : int =
    match x with
    | Paper -> 2
    | Rock -> 1
    | Scissors -> 3

let stringToShape (x: string) : Shape option =
    match x with
    | "A" -> Some Rock
    | "B" -> Some Paper
    | "C" -> Some Scissors
    | _ -> None

let score (x: Shape) (y: Outcome) : int = shapeToScore x + outcomeToScore y
