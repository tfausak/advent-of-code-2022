module AdventOfCode.Library.Day10.Part2

open AdventOfCode.Library.Day10.Part1

let solve (input: string) : string =
    input
    |> parseInstructions
    |> List.ofSeq
    |> evaluate 1
    |> Seq.mapi (fun i x -> i % 40 = x + 1 || i % 40 = x || i % 40 = x - 1)
    |> Seq.chunkBySize 40
    |> Seq.map (Seq.map (fun b -> if b then '#' else '.') >> System.String.Concat)
    |> String.concat "\n"
    |> printfn "%s"

    ""
