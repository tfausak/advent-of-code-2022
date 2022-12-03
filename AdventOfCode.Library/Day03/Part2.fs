module AdventOfCode.Library.Day03.Part2

open AdventOfCode.Library.Day03.Part1

let solve (input: string) : string =
    input.Split "\n"
    |> Seq.filter (System.String.IsNullOrWhiteSpace >> not)
    |> Seq.chunkBySize 3
    |> Seq.collect (Seq.map Set.ofSeq >> Set.intersectMany)
    |> Seq.sumBy priority
    |> string
