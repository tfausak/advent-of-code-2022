module AdventOfCode.Library.Day05.Part2

open AdventOfCode.Library.Common
open AdventOfCode.Library.Day05.Part1

let solve (input: string) : string =
    let (stacks, procedure) = parseInput input
    
    for (count, source, target) in procedure do
        let (before, after) = List.splitAt count stacks[source]
        stacks[source] <- after
        stacks[target] <- List.append before stacks[target]

    stacks |> Seq.map Seq.head |> System.String.Concat
