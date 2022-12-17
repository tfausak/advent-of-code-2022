module AdventOfCode.Library.Day12.Part1

// 464 too high
// 463 too high

open AdventOfCode.Library.Common

module Array2D =
    let rows (array: 'T[,]) : int seq = seq { 0 .. Array2D.length1 array - 1 }

    let cols (array: 'T[,]) : int seq = seq { 0 .. Array2D.length2 array - 1 }

    let indexes (array: 'T[,]) : (int * int) seq =
        Seq.allPairs (rows array) (cols array)

    let tryFindIndex (predicate: 'T -> bool) (array: 'T[,]) : (int * int) option =
        array |> indexes |> Seq.tryFind (fun (row, col) -> predicate array[row, col])

    let isInBounds (row : int, col : int) (array: 'T[,]) : bool =
        row >= 0
        && row < Array2D.length1 array
        && col >= 0
        && col < Array2D.length2 array

module Queue =
    type Queue<'T> = 'T list * 'T list

    let empty : 'T Queue = [], []

    let rec dequeue (q : 'T Queue) : ('T * 'T Queue) option =
        match fst q with
        | x :: xs -> Some (x, (xs, snd q))
        | [] ->
            let ys = snd q
            if List.isEmpty ys
            then None
            else dequeue (List.rev ys, [])

    let enqueue (x : 'T) (q : 'T Queue) : 'T Queue =
        fst q, x :: snd q

let rec flowWith (xs : int[,]) (frontier : (int * int) Queue.Queue) (distances : Map<int * int, int>) : Map<int * int, int> =
    match Queue.dequeue frontier with
    | None -> distances
    | Some ((row, col), queue) ->
        printfn $"at %A{(row, col)} %A{queue}"
        let distance = Map.find (row, col) distances
        let neighbors =
            [ row - 1, col // north
            ; row, col + 1 // east
            ; row + 1, col // south
            ; row, col - 1 // west
            ]
            |> List.filter (fun (r, c) ->
                if Array2D.isInBounds (r, c) xs
                then
                    let visited = distances.ContainsKey (r, c)
                    let valid = xs[row, col] <= xs[r, c] + 1
                    printfn $"to %A{(row, col)} [%A{xs[row, col]}] from %A{(r, c)} [%A{xs[r, c]}]: %A{visited}, %A{valid}"
                // not (distances.ContainsKey (r, c))
                // && Array2D.isInBounds (r, c) xs
                // && xs[row, col] <= xs[r, c] + 1
                    not visited && valid
                else false)
        let newFrontier = Seq.fold (fun a e -> Queue.enqueue e a) queue neighbors
        let newDistances = Seq.fold (fun a e -> Map.add e (distance + 1) a) distances neighbors
        flowWith xs newFrontier newDistances

let flow (start : int * int) (xs : int[,]) : Map<int * int, int> =
    flowWith xs (Queue.enqueue start Queue.empty) (Map.add start 0 Map.empty)

let solve (input: string) : string =
    let xs =
        input
        |> String.trimEnd
        |> String.split "\n"
        |> Array.map (fun x -> x.ToCharArray())
        |> array2D

    let source = xs |> Array2D.tryFindIndex (fun c -> c = 'S') |> Option.get
    let target = xs |> Array2D.tryFindIndex (fun c -> c = 'E') |> Option.get

    xs
    |> Array2D.map (fun c ->
        match c with
        | 'S' -> 0
        | 'E' -> 25
        | _ -> int c - int 'a')
    |> flow target
    |> Map.find source
    |> string
