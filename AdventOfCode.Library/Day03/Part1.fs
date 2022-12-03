module AdventOfCode.Library.Day03.Part1

let priority (x: char) : int =
    int x - if System.Char.IsUpper x then 38 else 96

let solve (input: string) : string =
    input.Split "\n"
    |> Seq.filter (System.String.IsNullOrWhiteSpace >> not)
    |> Seq.collect (fun line ->
        let mid = line.Length / 2
        let first = line.Substring(0, mid)
        let second = line.Substring mid
        let both = Set.intersect (Set.ofSeq first) (Set.ofSeq second)
        Set.toSeq both)
    |> Seq.sumBy priority
    |> string
