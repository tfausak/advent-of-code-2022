module AdventOfCode.Library.Day06.Part1

let solveWith (windowSize: int) (input: string) : string =
    input.TrimEnd()
    |> Seq.windowed windowSize
    |> Seq.mapi (fun i xs -> (i + windowSize, Set.count (Set.ofArray xs)))
    |> Seq.find (fun (_, n) -> n = windowSize)
    |> fst
    |> string

let solve (input: string) : string = solveWith 4 input
