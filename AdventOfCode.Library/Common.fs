module AdventOfCode.Library.Common

module Function =
    let uncurry (f: 'T -> 'U -> 'V) (x: 'T, y: 'U) : 'V = f x y

module String =
    let split (separator: string) (string: string) : string array = string.Split separator

module Tuple =
    let create (x: 'T) (y: 'U) : 'T * 'U = (x, y)

    let fromSeq (xs: 'T seq) : ('T * 'T) option =
        match Seq.tryItem 2 xs with
        | Some _ -> None
        | None -> Option.map2 create (Seq.tryItem 0 xs) (Seq.tryItem 1 xs)

    let map (f: 'T1 -> 'T2) (g: 'U1 -> 'U2) (x: 'T1, y: 'U1) : 'T2 * 'U2 = (f x, g y)

    let swap (x: 'T, y: 'U) : 'U * 'T = (y, x)
