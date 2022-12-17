module AdventOfCode.Library.Day14.Part2

open AdventOfCode.Library.Day14.Part1

let rec fall (m: Map<Point, Tile>) (y: int) (p: Point) : Point =
    let d = { p with Y = p.Y + 1 }

    if d.Y >= y then
        p
    else if Map.containsKey d m then
        let l = { d with X = p.X - 1 }

        if Map.containsKey l m then
            let r = { d with X = p.X + 1 }
            if Map.containsKey r m then p else fall m y r
        else
            fall m y l
    else
        fall m y d

let rec simulateWith (p: Point) (y: int) (m: Map<Point, Tile>) : Map<Point, Tile> =
    let q = fall m y p
    let n = Map.add q Sand m
    if q = p then n else simulateWith p y n

let simulate (p: Point) (m: Map<Point, Tile>) : Map<Point, Tile> =
    let y = m |> Map.keys |> Seq.map (fun x -> x.Y) |> Seq.max |> (fun x -> x + 2)
    simulateWith p y m

let solve (input: string) : string =
    input.TrimEnd().Split "\n"
    |> Seq.collect (fun line ->
        line.Split " -> "
        |> Seq.map (fun point ->
            match point.Split "," with
            | [| x; y |] -> { X = int x; Y = int y }
            | _ -> failwith $"invalid point: %A{point}")
        |> Seq.pairwise
        |> Seq.collect (fun (p, q) ->
            if p.X = q.X then
                seq { min p.Y q.Y .. max p.Y q.Y } |> Seq.map (fun y -> { p with Y = y })
            else if p.Y = q.Y then
                seq { min p.X q.X .. max p.X q.X } |> Seq.map (fun x -> { p with X = x })
            else
                failwith $"invalid pair: %A{p} %A{q}"))
    |> Seq.fold (fun m p -> Map.add p Rock m) Map.empty
    |> simulate { X = 500; Y = 0 }
    |> Map.filter (fun _ t -> t = Sand)
    |> Map.count
    |> string
