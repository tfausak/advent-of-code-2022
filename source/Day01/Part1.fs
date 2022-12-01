module Day01.Part1

let span (predicate: 'T -> bool) (source: 'T seq) : 'T seq * 'T seq =
    (Seq.takeWhile predicate source, Seq.skipWhile predicate source)

let rec spanAll (predicate: 'T -> bool) (source: 'T seq) : 'T seq seq =
    if Seq.isEmpty source then
        Seq.empty
    else
        let (xs, ys) = span predicate source
        Seq.append (Seq.singleton xs) (spanAll (predicate >> not) ys)

let even (number: int) : bool = number % 2 = 0

let solve (input: string) : string =
    input.Split "\n"
    |> spanAll (fun x -> x <> "")
    |> Seq.indexed
    |> Seq.filter (fst >> even)
    |> Seq.map (snd >> Seq.map int >> Seq.sum)
    |> Seq.max
    |> string
