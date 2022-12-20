module AdventOfCode.Library.Day16.Part1

open AdventOfCode.Library.Common

let tuple (x: 'T) (y: 'U) : 'T * 'U = x, y

let flip (f: 'T -> 'U -> 'V) (y: 'U) (x: 'T) : 'V = f x y

let between (lo: 'T) (hi: 'T) (x: 'T) : bool = lo <= x && x <= hi

let parseLeft (left: string) : (string * int) option =
    match left.Split " " with
    | [| _; valve; _; _; rate |] ->
        rate
        |> String.filter (between '0' '9')
        |> Int32.fromString
        |> Option.map (tuple valve)
    | _ -> None

let parseRight (right: string) : string seq =
    right
    |> String.filter ((<>) ',')
    |> fun string -> string.Split " "
    |> Seq.skip 4

let parseLine (line: string) : (string * (int * string seq)) option =
    match line.Split "; " with
    | [| left; right |] ->
        left
        |> parseLeft
        |> Option.map (fun (valve, rate) -> valve, (rate, parseRight right))
    | _ -> None

let parseInput (input: string) : Map<string, (int * string seq)> =
    input.TrimEnd().Split "\n" |> Seq.map (parseLine >> Option.get) |> Map.ofSeq

let rec bfsWith
    (table: Map<'T, 'T seq>)
    (frontier: 'T Queue.Queue)
    (cameFrom: Map<'T, 'T option>)
    : Map<'T, 'T option> =
    match Queue.dequeue frontier with
    | None -> cameFrom
    | Some(current, rest) ->
        let neighbors =
            table
            |> Map.tryFind current
            |> Option.defaultValue Seq.empty
            |> Seq.filter (fun next -> Map.containsKey next cameFrom |> not)

        let newFrontier = Seq.fold (fun a next -> Queue.enqueue next a) rest neighbors

        let newCameFrom =
            Seq.fold (fun a next -> Map.add next (Some current) a) cameFrom neighbors

        bfsWith table newFrontier newCameFrom

let bfs (target: 'T) (table: Map<'T, 'T seq>) : Map<'T, 'T option> =
    bfsWith table (Queue.enqueue target Queue.empty) (Map.add target None Map.empty)

let rec getPathWith (table: Map<'T, 'T option>) (current: 'T) (path: 'T list) : 'T list option =
    match Map.tryFind current table with
    | None -> None
    | Some None -> Some path
    | Some(Some next) -> getPathWith table next (current :: path)

let getPath (source: 'T) (table: Map<'T, 'T option>) : 'T list option = getPathWith table source []

let getCandidates (valves: Map<'T, int * 'T seq>) (current: 'T) (opened: 'T Set) (time: int) : Map<'T, int * int> =
    let vectorField = valves |> Map.map (fun _ (_, tunnels) -> tunnels) |> bfs current

    valves
    |> Map.keys
    |> Seq.filter (fun valve -> Set.contains valve opened |> not)
    |> Seq.choose (fun valve -> getPath valve vectorField |> Option.map (fun path -> valve, path))
    |> Seq.map (fun (valve, path) ->
        let rate = valves |> Map.tryFind valve |> Option.map fst |> Option.defaultValue 0
        let duration = List.length path + 1
        let pressure = rate * (time - duration)
        valve, (duration, pressure))
    |> Seq.filter (fun (_, (_, pressure)) -> pressure > 0)
    |> Map.ofSeq

let rec getPressuresWith
    (valves: Map<'T, int * 'T seq>)
    (current: 'T)
    (opened: 'T Set)
    (time: int)
    (pressure: int)
    : int seq =
    if time < 0 then
        Seq.empty
    else
        let candidates = getCandidates valves current opened time

        if time = 0 || Seq.isEmpty candidates then
            Seq.singleton pressure
        else
            candidates
            |> Map.toSeq
            |> Seq.collect (fun (valve, (duration, p)) ->
                getPressuresWith valves valve (Set.add valve opened) (time - duration) (pressure + p))

let getPressures (current: 'T) (time: int) (pressure: int) (valves: Map<'T, int * 'T seq>) : int seq =
    getPressuresWith valves current Set.empty time pressure

let solve (input: string) : string =
    input |> parseInput |> getPressures "AA" 30 0 |> Seq.max |> string
