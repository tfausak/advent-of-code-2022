module AdventOfCode.Library.Day13.Part1

module Parser =
    type Parser<'T> = char list -> ('T * char list) option

    let map (f: 'T -> 'U) (p: 'T Parser) : 'U Parser =
        fun s1 -> p s1 |> Option.map (fun (t, s2) -> f t, s2)

    let succeed (t: 'T) : 'T Parser = fun s -> Some(t, s)

    let lift (f: 'T -> 'U -> 'V) (p: 'T Parser) (q: 'U Parser Lazy) : 'V Parser =
        fun s1 ->
            match p s1 with
            | None -> None
            | Some(t, s2) ->
                match q.Force () s2 with
                | None -> None
                | Some(u, s3) -> Some(f t u, s3)

    let choose (p: 'T Parser) (u: 'T Parser Lazy) : 'T Parser =
        fun s1 ->
            match p s1 with
            | None -> u.Force () s1
            | Some x -> Some x

    let satisfy (f: char -> bool) : char Parser =
        fun s1 ->
            match s1 with
            | [] -> None
            | c :: s2 -> if f c then Some(c, s2) else None

    let char (c: char) : char Parser = satisfy ((=) c)

    let between (l: 'T Parser) (r: 'U Parser) (p: 'V Parser) : 'V Parser =
        lift (fun _ v -> v) l (lazy (lift (fun v _ -> v) p (lazy r)))

    let rec some (p: 'T Parser) : 'T list Parser =
        lift (fun t ts -> t :: ts) p (lazy (many p))

    and many (p: 'T Parser) : 'T list Parser = choose (some p) (lazy (succeed []))

    let sepBy (s: 'T Parser) (p: 'U Parser) : 'U list Parser =
        choose (lift (fun u us -> u :: us) p (lazy (many (lift (fun _ u -> u) s (lazy p))))) (lazy (succeed []))

type Packet =
    | Integer of int
    | List of Packet list

let rec packetParser () : Packet Parser.Parser =
    Parser.choose
        (Parser.satisfy (fun c -> '0' <= c && c <= '9')
         |> Parser.some
         |> Parser.map (Seq.fold (fun n d -> n * 10 + (int d - int '0')) 0 >> Integer))
        (lazy
            (packetParser ()
             |> Parser.sepBy (Parser.char ',')
             |> Parser.between (Parser.char '[') (Parser.char ']')
             |> Parser.map List))

let stringToPacket (s: string) : Packet option =
    match s.ToCharArray() |> List.ofArray |> packetParser () with
    | Some(p, []) -> Some p
    | _ -> None

type Ordering =
    | LT
    | EQ
    | GT

let rec comparePackets (left: Packet) (right: Packet) : Ordering =
    match left, right with
    | Integer li, Integer ri ->
        match compare li ri with
        | -1 -> LT
        | 1 -> GT
        | _ -> EQ
    | Integer _, List _ -> comparePackets (List [ left ]) right
    | List _, Integer _ -> comparePackets left (List [ right ])
    | List [], List [] -> EQ
    | List [], List _ -> LT
    | List _, List [] -> GT
    | List(lh :: lt), List(rh :: rt) ->
        match comparePackets lh rh with
        | LT -> LT
        | EQ -> comparePackets (List lt) (List rt)
        | GT -> GT

let solve (input: string) : string =
    input.TrimEnd().Split "\n\n"
    |> Seq.indexed
    |> Seq.filter (fun (_, pair) ->
        match pair.Split "\n" with
        | [| left; right |] ->
            let lp = left |> stringToPacket |> Option.get
            let rp = right |> stringToPacket |> Option.get
            comparePackets lp rp = LT
        | _ -> false)
    |> Seq.sumBy (fun (index, _) -> index + 1)
    |> string
