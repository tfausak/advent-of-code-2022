module AdventOfCode.Library.Day09.Part2

open AdventOfCode.Library.Common
open AdventOfCode.Library.Day09.Part1

let solve (input: string) : string =
    input
    |> parseInput
    |> moveHead
    |> Function.iterate 9 moveTail
    |> Set.ofSeq
    |> Set.count
    |> string
