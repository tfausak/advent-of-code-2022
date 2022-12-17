module AdventOfCode.Library.Common

module Function =
    let rec iterate (n: int) (f: 'T -> 'T) (x: 'T) : 'T =
        if n < 1 then x else iterate (n - 1) f (f x)

    let uncurry (f: 'T -> 'U -> 'V) (x: 'T, y: 'U) : 'V = f x y

module Int32 =
    let fromString (string: string) : int option =
        match System.Int32.TryParse string with
        | true, n -> Some n
        | _ -> None

module String =
    let split (separator: string) (string: string) : string array = string.Split separator

    let trimEnd (string: string) : string = string.TrimEnd()

    let trimStart (string: string) : string = string.TrimStart()

module Tuple =
    let create (x: 'T) (y: 'U) : 'T * 'U = (x, y)

    let fromSeq (xs: 'T seq) : ('T * 'T) option =
        match Seq.tryItem 2 xs with
        | Some _ -> None
        | None -> Option.map2 create (Seq.tryItem 0 xs) (Seq.tryItem 1 xs)

    let map (f: 'T1 -> 'T2) (g: 'U1 -> 'U2) (x: 'T1, y: 'U1) : 'T2 * 'U2 = (f x, g y)

    let swap (x: 'T, y: 'U) : 'U * 'T = (y, x)
