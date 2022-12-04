module AdventOfCode.Library.Day04.Part1

open AdventOfCode.Library.Common

let solve (input : string) : string =
    input.Split "\n"
    |> Seq.filter (System.String.IsNullOrWhiteSpace >> not)
    |> Seq.map (fun line -> line.Split ",")
    |> Seq.map (Seq.map (fun line -> line.Split "-"))
    |> Seq.map (Seq.map (Seq.map int))
    |> Seq.map (Seq.map (seqToTuple >> Option.get))
    |> Seq.map (seqToTuple >> Option.get)
    |> Seq.filter (fun ((xl, xh), (yl, yh)) -> xl <= yl && xh >= yh || yl <= xl && yh >= xh)
    |> Seq.length
    |> string
