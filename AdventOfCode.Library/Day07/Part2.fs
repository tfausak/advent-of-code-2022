module AdventOfCode.Library.Day07.Part2

open AdventOfCode.Library.Day07.Part1

let solve (input: string) =
    let files = getFiles input
    let threshold = Seq.sumBy (snd >> snd) files - 40000000

    getSizes files
    |> Seq.filter (fun size -> size >= threshold)
    |> Seq.sort
    |> Seq.head
    |> string
