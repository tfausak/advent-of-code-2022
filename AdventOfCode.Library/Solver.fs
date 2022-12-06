module AdventOfCode.Library.Solver

let solve (day: int) (part: int) (input: string) : string option =
    match (day, part) with
    | (1, 1) -> Some <| Day01.Part1.solve input
    | (1, 2) -> Some <| Day01.Part2.solve input
    | (2, 1) -> Some <| Day02.Part1.solve input
    | (2, 2) -> Some <| Day02.Part2.solve input
    | (3, 1) -> Some <| Day03.Part1.solve input
    | (3, 2) -> Some <| Day03.Part2.solve input
    | (4, 1) -> Some <| Day04.Part1.solve input
    | (4, 2) -> Some <| Day04.Part2.solve input
    | (5, 1) -> Some <| Day05.Part1.solve input
    | (5, 2) -> Some <| Day05.Part2.solve input
    | _ -> None