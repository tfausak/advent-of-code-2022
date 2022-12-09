module AdventOfCode.Library.Day09.Part1

open AdventOfCode.Library.Common

type Direction =
    | D // down
    | L // left
    | R // right
    | U // up

let stringToDirection (string: string) : Direction option =
    match string with
    | "D" -> Some D
    | "L" -> Some L
    | "R" -> Some R
    | "U" -> Some U
    | _ -> None

let origin = (0, 0)

let solve (input: string) : string =
    input.TrimEnd().Split "\n"
    |> Seq.collect (
        String.split " "
        >> Tuple.fromSeq
        >> Option.get
        >> Tuple.map (stringToDirection >> Option.get) int
        >> Tuple.swap
        >> Function.uncurry Seq.replicate
    )
    |> Seq.scan
        (fun (x, y) direction ->
            match direction with
            | D -> (x, y - 1)
            | L -> (x - 1, y)
            | R -> (x + 1, y)
            | U -> (x, y + 1))
        origin
    |> Seq.scan
        (fun (tx, ty) (hx, hy) ->
            if tx = hx && ty + 2 = hy then (tx, ty + 1)
            else if tx = hx && ty - 2 = hy then (tx, ty - 1)
            else if ty = hy && tx + 2 = hx then (tx + 1, ty)
            else if ty = hy && tx - 2 = hx then (tx - 1, ty)
            else if tx + 1 = hx && ty + 2 = hy then (tx + 1, ty + 1)
            else if tx + 2 = hx && ty + 1 = hy then (tx + 1, ty + 1)
            else if tx - 1 = hx && ty + 2 = hy then (tx - 1, ty + 1)
            else if tx - 2 = hx && ty + 1 = hy then (tx - 1, ty + 1)
            else if tx + 1 = hx && ty - 2 = hy then (tx + 1, ty - 1)
            else if tx + 2 = hx && ty - 1 = hy then (tx + 1, ty - 1)
            else if tx - 1 = hx && ty - 2 = hy then (tx - 1, ty - 1)
            else if tx - 2 = hx && ty - 1 = hy then (tx - 1, ty - 1)
            else (tx, ty))
        origin
    |> Set.ofSeq
    |> Set.count
    |> string
