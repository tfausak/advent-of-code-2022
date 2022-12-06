module AdventOfCode.Library.Day01.Part2

let solve (input: string) : string =
    input.TrimEnd().Split "\n\n"
    |> Seq.map (fun chunk -> chunk.Split "\n" |> Seq.sumBy int)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum
    |> string
