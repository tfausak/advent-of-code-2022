module AdventOfCode.Library.Day15.Part2

open AdventOfCode.Library.Day15.Part1

let solve (input: string) : string =
    let sensorToBeacon = input |> parseInput |> Map.ofSeq

    sensorToBeacon
    |> Map.toSeq
    |> Seq.tryPick (fun (sensor, beacon) ->
        let radius = distance sensor beacon + 1
        let limit = 4000000
        let otherSensors = Map.filter (fun s _ -> s <> sensor) sensorToBeacon

        seq { -radius .. radius }
        |> Seq.collect (fun dx ->
            let dy = radius - abs dx

            [ { X = sensor.X + dx; Y = sensor.Y + dy }
              { X = sensor.X + dx; Y = sensor.Y - dy } ])
        |> Seq.filter (fun p ->
            p.X >= 0
            && p.X <= limit
            && p.Y >= 0
            && p.Y <= limit
            && Map.forall (fun s b -> distance s p > distance s b) otherSensors)
        |> Seq.tryHead)
    |> Option.get
    |> fun p -> 4_000_000L * int64 p.X + int64 p.Y
    |> string
