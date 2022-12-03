module AdventOfCode.Library.Day03.Part1

let solve (input: string) : string =
    input.Split "\n"
    |> Seq.filter (System.String.IsNullOrWhiteSpace >> not)
    |> Seq.collect (fun line ->
        let mid = line.Length / 2
        let first = line.Substring(0, mid)
        let second = line.Substring mid
        let both = Set.intersect (Set.ofSeq first) (Set.ofSeq second)
        Set.toSeq both)
    |> Seq.sumBy (fun x ->
        if System.Char.IsUpper x then
            int x - int 'A' + 27
        else
            int x - int 'a' + 1)
    |> string
