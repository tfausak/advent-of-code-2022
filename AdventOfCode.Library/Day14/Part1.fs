module AdventOfCode.Library.Day14.Part1

type Point = { X: int; Y: int }

type Tile =
    | Rock
    | Sand

let rec fall (p: Point) (m: Map<Point, Tile>) : Point option =
    let maxY = m |> Map.keys |> Seq.map (fun q -> q.Y) |> Seq.max

    if p.Y >= maxY then
        None
    else
        let down = { p with Y = p.Y + 1 }

        if Map.containsKey down m then
            let left = { down with X = p.X - 1 }

            if Map.containsKey left m then
                let right = { down with X = p.X + 1 }
                if Map.containsKey right m then Some p else fall right m
            else
                fall left m
        else
            fall down m

let rec simulate (p: Point) (m: Map<Point, Tile>) : Map<Point, Tile> =
    match fall p m with
    | None -> m
    | Some q -> simulate p (Map.add q Sand m)

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
