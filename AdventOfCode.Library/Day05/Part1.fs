module AdventOfCode.Library.Day05.Part1

open AdventOfCode.Library.Common

let skipLast (source: 'T seq) : 'T seq = Seq.take (Seq.length source - 1) source

let parseInput (input: string) : (char list array) * ((int * int * int) seq) =
    let (rawStacks, rawProcedure) = input.TrimEnd().Split "\n\n" |> seqToTuple |> Option.get

    let stacks =
        rawStacks.Split "\n"
        |> Seq.map (Seq.chunkBySize 4 >> Seq.map (Seq.item 1))
        |> Seq.transpose
        |> Seq.map (Seq.filter (fun crate -> crate <> ' ') >> skipLast >> Seq.toList)
        |> Seq.toArray

    let procedure =
        rawProcedure.Split "\n"
        |> Seq.map (fun line ->
            match line.Split " " with
            | [| "move"; count; "from"; source; "to"; target |] -> (int count, int source - 1, int target - 1)
            | step -> failwith $"invalid step: %A{step}")

    (stacks, procedure)

let solve (input: string) : string =
    let (stacks, procedure) = parseInput input

    for (count, source, target) in procedure do
        for _ = 1 to count do
            match stacks[source] with
            | h :: t ->
                stacks[source] <- t
                stacks[target] <- h :: stacks[target]
            | _ -> failwith "empty source"

    stacks |> Seq.map Seq.head |> System.String.Concat
