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

let parseInput (input: string) : Direction seq =
    input.TrimEnd().Split "\n"
    |> Seq.collect (
        String.split " "
        >> Tuple.fromSeq
        >> Option.get
        >> Tuple.map (stringToDirection >> Option.get) int
        >> Tuple.swap
        >> Function.uncurry Seq.replicate
    )

let moveHead (directions: Direction seq) : (int * int) seq =
    directions
    |> Seq.scan
        (fun (x, y) direction ->
            match direction with
            | D -> (x, y - 1)
            | L -> (x - 1, y)
            | R -> (x + 1, y)
            | U -> (x, y + 1))
        origin

let moveTail (heads: (int * int) seq) : (int * int) seq =
    heads
    |> Seq.scan
        (fun (tx, ty) (hx, hy) ->
            let dx = hx - tx
            let dy = hy - ty

            if abs dx > 1 || abs dy > 1 then
                (tx + sign dx, ty + sign dy)
            else
                (tx, ty))
        origin

let solve (input: string) : string =
    input |> parseInput |> moveHead |> moveTail |> Set.ofSeq |> Set.count |> string
