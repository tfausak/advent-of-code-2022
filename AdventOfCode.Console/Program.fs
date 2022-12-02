open AdventOfCode.Library

[<EntryPoint>]
let main (arguments: string[]) : int =
    let input = stdin.ReadToEnd()

    let output =
        match arguments with
        | [| "1"; "1" |] -> Day01.Part1.solve input
        | [| "1"; "2" |] -> Day01.Part2.solve input
        | [| "2"; "1" |] -> Day02.Part1.solve input
        | _ -> failwith $"unknown arguments: %A{arguments}"

    printfn $"%s{output}"
    0
