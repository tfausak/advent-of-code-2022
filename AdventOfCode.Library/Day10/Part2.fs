module AdventOfCode.Library.Day10.Part2

open AdventOfCode.Library.Day10.Part1

let encode (input: bool seq) : int =
    input
    |> Seq.map (fun b -> if b then 1 else 0)
    |> Seq.mapi (fun i n -> n * pown 2 i)
    |> Seq.sum

let identify (input: bool seq seq) : char option =
    match input |> Seq.map encode |> Seq.toArray with
    | [| 49; 41; 37; 35; 0 |] -> Some 'Z'
    | [| 63; 9; 25; 38; 0 |] -> Some 'R'
    | [| 62; 9; 9; 62; 0 |] -> Some 'A'
    | [| 63; 32; 32; 32; 0 |] -> Some 'L'
    | [| 63; 5; 5; 1; 0 |] -> Some 'F'
    | [| 31; 32; 32; 31; 0 |] -> Some 'U'
    | _ -> None

let solve (input: string) : string =
    input
    |> parseInstructions
    |> List.ofSeq
    |> evaluate 1
    |> Seq.mapi (fun i x -> i % 40 = x + 1 || i % 40 = x || i % 40 = x - 1)
    |> Seq.chunkBySize 40
    |> Seq.transpose
    |> Seq.chunkBySize 5
    |> Seq.map (identify >> Option.get)
    |> System.String.Concat
    |> string
