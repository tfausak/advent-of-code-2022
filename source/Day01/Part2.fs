module Day01.Part2

let solve (input: string) : string =
    input.Split "\n\n"
    |> Seq.map (fun chunk -> chunk.Split "\n" |> Seq.filter (fun line -> line <> "") |> Seq.sumBy int)
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum
    |> string
