module AdventOfCode.Library.Day03.Part1

let solve (input : string) : string =
    input.Split "\n"
    |> Seq.filter (System.String.IsNullOrWhiteSpace >> not)
    |> Seq.map (fun line ->
        let mid = line.Length / 2
        (line.Substring (0, mid), line.Substring mid))
    |> Seq.map (fun (x, y) -> (Set.ofSeq x, Set.ofSeq y))
    |> Seq.map (fun (x, y) -> Set.intersect x y)
    |> Seq.map Set.toSeq
    |> Seq.concat
    |> Seq.map (fun x ->
        if System.Char.IsUpper x
        then int x - int 'A' + 27
        else int x - int 'a' + 1)
    |> Seq.sum
    |> string
