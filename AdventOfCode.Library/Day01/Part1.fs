module AdventOfCode.Library.Day01.Part1

let solve (input: string) : string =
    input.TrimEnd().Split "\n\n"
    |> Seq.map (fun chunk -> chunk.Split "\n" |> Seq.sumBy int)
    |> Seq.max
    |> string
