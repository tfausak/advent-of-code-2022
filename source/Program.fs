exception UnknownArgumentsException of string[]

[<EntryPoint>]
let main (arguments: string[]) : int =
    let input = stdin.ReadToEnd()

    let output =
        match arguments with
        | [| "1"; "1" |] -> Day01.Part1.solve input
        | _ -> raise (UnknownArgumentsException arguments)

    printfn $"%s{output}"
    0
