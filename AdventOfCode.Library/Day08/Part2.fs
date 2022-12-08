module AdventOfCode.Library.Day08.Part2

open AdventOfCode.Library.Day08.Part1

let scoreN (map: int[,]) (row: int) (col: int) : int =
    seq { row - 1 .. -1 .. 0 }
    |> Seq.fold (fun (s, b) r -> (s + (if b then 0 else 1), b || map[r, col] >= map[row, col])) (0, false)
    |> fst

let scoreE (map: int[,]) (row: int) (col: int) : int =
    seq { col + 1 .. Array2D.length2 map - 1 }
    |> Seq.fold (fun (s, b) c -> (s + (if b then 0 else 1), b || map[row, c] >= map[row, col])) (0, false)
    |> fst

let scoreS (map: int[,]) (row: int) (col: int) : int =
    seq { row + 1 .. Array2D.length1 map - 1 }
    |> Seq.fold (fun (s, b) r -> (s + (if b then 0 else 1), b || map[r, col] >= map[row, col])) (0, false)
    |> fst

let scoreW (map: int[,]) (row: int) (col: int) : int =
    seq { col - 1 .. -1 .. 0 }
    |> Seq.fold (fun (s, b) c -> (s + (if b then 0 else 1), b || map[row, c] >= map[row, col])) (0, false)
    |> fst

let score (map: int[,]) (row: int) (col: int) : int =
    [ scoreN; scoreE; scoreS; scoreW ]
    |> Seq.map (fun f -> f map row col)
    |> Seq.fold (*) 1

let solve (input: string) : string =
    let map = parseInput input

    seq {
        for row in 0 .. Array2D.length1 map - 1 do
            for col in 0 .. Array2D.length2 map - 1 -> (row, col)
    }
    |> Seq.map (fun (row, col) -> score map row col)
    |> Seq.max
    |> string
