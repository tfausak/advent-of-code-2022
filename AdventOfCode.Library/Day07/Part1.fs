module AdventOfCode.Library.Day07.Part1

type Action =
    | ChangeDirectory of string
    | List
    | Directory of string
    | File of int * string

let getFiles (input: string) : (string list * (string * int)) list =
    input.TrimEnd().Split "\n"
    |> Seq.map (fun line ->
        match line.Split " " with
        | [| "$"; "cd"; name |] -> ChangeDirectory name
        | [| "$"; "ls" |] -> List
        | [| "dir"; name |] -> Directory name
        | [| size; name |] -> File(int size, name)
        | _ -> failwith $"invalid line: %A{line}")
    |> Seq.fold
        (fun (path, files) action ->
            match action with
            | ChangeDirectory name ->
                match name with
                | "/" -> ([], files)
                | ".." -> (List.tail path, files)
                | _ -> (name :: path, files)
            | List -> (path, files)
            | Directory _ -> (path, files)
            | File(size, name) -> (path, (List.rev path, (name, size)) :: files))
        ([], [])
    |> snd

let rec hasPrefix (prefix: 'T list) (list: 'T list) : bool =
    match prefix, list with
    | [], _ -> true
    | ph :: pt, lh :: lt when ph = lh -> hasPrefix pt lt
    | _ -> false

let rec prefixes (list: 'T list) : 'T list list =
    match list with
    | [] -> []
    | h :: t -> [ h ] :: List.map (fun x -> h :: x) (prefixes t)

let getSizes (files: (string list * (string * int)) seq) : int seq =
    files
    |> Seq.map fst
    |> Seq.collect prefixes
    |> Set.ofSeq
    |> Seq.map (fun path -> files |> Seq.filter (fst >> hasPrefix path) |> Seq.sumBy (snd >> snd))

let solve (input: string) =
    getSizes (getFiles input)
    |> Seq.filter (fun size -> size <= 100000)
    |> Seq.sum
    |> string
