module AdventOfCode.Library.Day12.Part1

open AdventOfCode.Library.Common

module Array2D =
    let tryFindIndex (predicate: 'T -> bool) (array: 'T[,]) : (int * int) option =
        Seq.allPairs (seq { 0 .. Array2D.length1 array - 1 }) (seq { 0 .. Array2D.length2 array - 1 })
        |> Seq.tryFind (fun (row, col) -> predicate array[row, col])

    let isInBounds (row: int, col: int) (array: 'T[,]) : bool =
        row >= 0
        && row < Array2D.length1 array
        && col >= 0
        && col < Array2D.length2 array

let rec flowWith
    (xs: int[,])
    (frontier: (int * int) Queue.Queue)
    (distances: Map<int * int, int>)
    : Map<int * int, int> =
    match Queue.dequeue frontier with
    | None -> distances
    | Some((row, col), queue) ->
        let distance = Map.find (row, col) distances

        let neighbors =
            [ row - 1, col // north
              row, col + 1 // east
              row + 1, col // south
              row, col - 1 ] // west
            |> List.filter (fun (r, c) ->
                Array2D.isInBounds (r, c) xs
                && not (distances.ContainsKey(r, c))
                && xs[row, col] <= xs[r, c] + 1)

        let newFrontier = Seq.fold (fun a e -> Queue.enqueue e a) queue neighbors

        let newDistances =
            Seq.fold (fun a e -> Map.add e (distance + 1) a) distances neighbors

        flowWith xs newFrontier newDistances

let flow (start: int * int) (xs: int[,]) : Map<int * int, int> =
    flowWith xs (Queue.enqueue start Queue.empty) (Map.add start 0 Map.empty)

let solve (input: string) : string =
    let heightmap =
        input
        |> String.trimEnd
        |> String.split "\n"
        |> Array.map (fun x -> x.ToCharArray())
        |> array2D

    let source = heightmap |> Array2D.tryFindIndex (fun c -> c = 'S') |> Option.get
    let target = heightmap |> Array2D.tryFindIndex (fun c -> c = 'E') |> Option.get

    heightmap
    |> Array2D.map (fun c ->
        match c with
        | 'S' -> 0
        | 'E' -> 25
        | _ -> int c - int 'a')
    |> flow target
    |> Map.find source
    |> string
