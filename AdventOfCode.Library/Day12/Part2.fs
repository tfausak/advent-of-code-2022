module AdventOfCode.Library.Day12.Part2

open AdventOfCode.Library.Common
open AdventOfCode.Library.Day12.Part1

let solve (input: string) : string =
    let heights =
        input
        |> String.trimEnd
        |> String.split "\n"
        |> Array.map (fun x -> x.ToCharArray())
        |> array2D

    let target = heights |> Array2D.tryFindIndex (fun c -> c = 'E') |> Option.get

    let distances =
        heights
        |> Array2D.map (fun c ->
            match c with
            | 'S' -> 0
            | 'E' -> 25
            | _ -> int c - int 'a')
        |> flow target

    Seq.allPairs (seq { 0 .. Array2D.length1 heights - 1 }) (seq { 0 .. Array2D.length2 heights - 1 })
    |> Seq.filter (fun (row, col) -> heights[row, col] = 'S' || heights[row, col] = 'a')
    |> Seq.choose (fun index -> Map.tryFind index distances)
    |> Seq.min
    |> string
