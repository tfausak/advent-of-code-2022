module AdventOfCode.Library.Day03.Part2

let priority (x: char) : int =
    if System.Char.IsUpper x then
        int x - int 'A' + 27
    else
        int x - int 'a' + 1

let solve (input: string) : string =
    input.Split "\n"
    |> Seq.filter (System.String.IsNullOrWhiteSpace >> not)
    |> Seq.chunkBySize 3
    |> Seq.collect (Seq.map Set.ofSeq >> Set.intersectMany)
    |> Seq.sumBy priority
    |> string
