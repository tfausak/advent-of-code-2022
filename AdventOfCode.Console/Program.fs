open AdventOfCode.Library.Solver

[<EntryPoint>]
let main (arguments: string array) : int =
    let (day, part) =
        match arguments with
        | [| d; p |] -> (int d, int p)
        | _ -> failwith $"I did not expect these arguments: %A{arguments}."

    let input = stdin.ReadToEnd()

    match solve day part input with
    | None -> failwith $"I don't know how to solve day %A{day} part %A{part}."
    | Some output -> printfn $"%s{output}"

    0
