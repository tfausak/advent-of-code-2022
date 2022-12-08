module AdventOfCode.Library.Day08.Part1

let parseInput (input: string) : int[,] =
    input.TrimEnd().Split "\n"
    |> Seq.map (Seq.map (fun char -> int char - int '0'))
    |> array2D

let isVisibleN (map: int[,]) (row: int) (col: int) : bool =
    Seq.forall (fun r -> map[r, col] < map[row, col]) (seq { 0 .. row - 1 })

let isVisibleE (map: int[,]) (row: int) (col: int) : bool =
    Seq.forall (fun c -> map[row, c] < map[row, col]) (seq { col + 1 .. Array2D.length2 map - 1 })

let isVisibleS (map: int[,]) (row: int) (col: int) : bool =
    Seq.forall (fun r -> map[r, col] < map[row, col]) (seq { row + 1 .. Array2D.length1 map - 1 })

let isVisibleW (map: int[,]) (row: int) (col: int) : bool =
    Seq.forall (fun c -> map[row, c] < map[row, col]) (seq { 0 .. col - 1 })

let isVisible (map: int[,]) (row: int) (col: int) : bool =
    Seq.exists (fun f -> f map row col) [ isVisibleN; isVisibleE; isVisibleS; isVisibleW ]

let solve (input: string) : string =
    let map = parseInput input

    seq {
        for row in 0 .. Array2D.length1 map - 1 do
            for col in 0 .. Array2D.length2 map - 1 -> (row, col)
    }
    |> Seq.filter (fun (row, col) -> isVisible map row col)
    |> Seq.length
    |> string
