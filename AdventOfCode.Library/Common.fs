module AdventOfCode.Library.Common

let seqToTuple (xs: 'T seq) : ('T * 'T) option =
    match Seq.tryItem 2 xs with
    | Some _ -> None
    | None -> Option.map2 (fun x y -> (x, y)) (Seq.tryItem 0 xs) (Seq.tryItem 1 xs)
