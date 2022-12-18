module AdventOfCode.Library.Day15.Part1

open AdventOfCode.Library.Common

type Point = { X: int; Y: int }

let parseCoordinate (s: string) : int option =
    s
    |> String.filter (fun c -> ('0' <= c && c <= '9') || c = '-')
    |> Int32.fromString

let parsePoint (s1: string) (s2: string) : Point option =
    Option.map2 (fun x y -> { X = x; Y = y }) (parseCoordinate s1) (parseCoordinate s2)

let parseLine (s: string) : (Point * Point) option =
    match s.Split " " with
    | [| _; _; sx; sy; _; _; _; _; bx; by |] -> Option.map2 (fun sp bp -> sp, bp) (parsePoint sx sy) (parsePoint bx by)
    | _ -> None

let parseInput (s: string) : (Point * Point) seq =
    s.TrimEnd().Split "\n" |> Seq.map (parseLine >> Option.get)

let distance (p1: Point) (p2: Point) : int = abs (p1.X - p2.X) + abs (p1.Y - p2.Y)

let solve (input: string) : string =
    let sensorToBeacon = input |> parseInput |> Map.ofSeq

    let nonBeacons =
        sensorToBeacon
        |> Map.toSeq
        |> Seq.collect (fun (sensor, beacon) ->
            let distanceToBeacon = distance sensor beacon
            let point = { sensor with Y = 2000000 }
            let distanceToPoint = distance sensor point
            let delta = distanceToBeacon - distanceToPoint

            if delta < 0 then
                Seq.empty
            else
                seq { point.X - delta .. point.X + delta }
                |> Seq.map (fun x -> { point with X = x }))
        |> Set.ofSeq

    sensorToBeacon
    |> Map.values
    |> Set.ofSeq
    |> Set.difference nonBeacons
    |> Set.count
    |> string
