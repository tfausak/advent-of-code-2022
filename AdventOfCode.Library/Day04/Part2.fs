module AdventOfCode.Library.Day04.Part2

open AdventOfCode.Library.Common

let overlaps ((xl: 'T, xh: 'T), (yl: 'T, yh: 'T)) : bool = xh >= yl && xl <= yh

let solve (input: string) : string =
    input.Split "\n"
    |> Seq.filter (fun line -> line <> "")
    |> Seq.map (fun l1 ->
        l1.Split ","
        |> Seq.map (fun l2 -> l2.Split "-" |> Seq.map int |> seqToTuple |> Option.get)
        |> seqToTuple
        |> Option.get)
    |> Seq.filter overlaps
    |> Seq.length
    |> string
