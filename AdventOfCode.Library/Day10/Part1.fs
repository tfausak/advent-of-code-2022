module AdventOfCode.Library.Day10.Part1

open AdventOfCode.Library.Common

type Instruction =
    | Addx of int
    | Noop

let parseInstruction (input: string) : Instruction option =
    match String.split " " input with
    | [| "addx"; v |] -> Option.map Addx (Int32.fromString v)
    | [| "noop" |] -> Some Noop
    | _ -> None

let parseInstructions (input: string) : Instruction seq =
    input
    |> String.trimEnd
    |> String.split "\n"
    |> Seq.map (parseInstruction >> Option.get)

let rec evaluate (register: int) (instructions: Instruction list) : int list =
    match instructions with
    | [] -> [ register ]
    | i :: is ->
        match i with
        | Addx v -> register :: register :: evaluate (register + v) is
        | Noop -> register :: evaluate register is

let solve (input: string) : string =
    input
    |> parseInstructions
    |> List.ofSeq
    |> evaluate 1
    |> Seq.indexed
    |> Seq.filter (fun (i, _) -> (i - 19) % 40 = 0)
    |> Seq.sumBy (fun (i, x) -> (i + 1) * x)
    |> string
