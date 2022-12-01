module AdventOfCode.Library.Day01.Part1

let solve (input: string) : string =
    input.Split "\n\n"
    |> Seq.map (fun chunk -> chunk.Split "\n" |> Seq.filter (fun line -> line <> "") |> Seq.sumBy int)
    |> Seq.max
    |> string
