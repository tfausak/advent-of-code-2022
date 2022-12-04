module AdventOfCode.Library.Day04.Part1

open AdventOfCode.Library.Common

let contains (inner: 'T * 'U) (outer: 'T * 'U) : bool =
    fst outer <= fst inner && snd outer >= snd inner

let solve (input: string) : string =
    input.Split "\n"
    |> Seq.filter (fun line -> line <> "")
    |> Seq.map (fun l1 ->
        l1.Split ","
        |> Seq.map (fun l2 -> l2.Split "-" |> Seq.map int |> seqToTuple |> Option.get)
        |> seqToTuple
        |> Option.get)
    |> Seq.filter (fun (x, y) -> contains x y || contains y x)
    |> Seq.length
    |> string
