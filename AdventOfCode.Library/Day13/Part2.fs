module AdventOfCode.Library.Day13.Part2

open AdventOfCode.Library.Day13.Part1

let divider1 = List [ List [ Integer 2 ] ]

let divider2 = List [ List [ Integer 6 ] ]

let solve (input: string) : string =
    input.TrimEnd().Split "\n\n"
    |> Seq.collect (fun pair -> pair.Split "\n" |> Seq.map (stringToPacket >> Option.get))
    |> Seq.append (seq [ divider1; divider2 ])
    |> Seq.sortWith (fun l r ->
        match comparePackets l r with
        | LT -> -1
        | EQ -> 0
        | GT -> 1)
    |> Seq.indexed
    |> Seq.filter (fun (_, p) -> p = divider1 || p = divider2)
    |> Seq.fold (fun n (i, _) -> n * (i + 1)) 1
    |> string
