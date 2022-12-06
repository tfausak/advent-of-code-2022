module AdventOfCode.Library.Day06.Part1

let solve (input: string) : string =
    input.TrimEnd()
    |> Seq.windowed 4
    |> Seq.mapi (fun i xs -> (i + 4, Set.count (Set.ofArray xs)))
    |> Seq.find (fun (_, n) -> n = 4)
    |> fst
    |> string
