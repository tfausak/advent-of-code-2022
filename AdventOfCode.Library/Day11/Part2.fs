module AdventOfCode.Library.Day11.Part2

open AdventOfCode.Library.Common
open AdventOfCode.Library.Day11.Part1

let simulateRound (monkeys: Monkey array) : Monkey array =
    let m = Seq.fold (*) 1 (Seq.map (fun monkey -> monkey.Divisor) monkeys)

    for monkey in monkeys do
        for l1 in monkey.Items do
            let n =
                match monkey.Operand with
                | Old -> l1
                | Value x -> x

            let l2 =
                match monkey.Operation with
                | Add -> int64 l1 + int64 n
                | Multiply -> int64 l1 * int64 n

            let l3 = int (l2 % int64 m)

            let i =
                if divisibleBy monkey.Divisor l3 then
                    monkey.Truthy
                else
                    monkey.Falsey

            monkeys[i].Items <- Seq.append monkeys[i].Items (Seq.singleton l3)

        monkey.Inspections <- monkey.Inspections + Seq.length monkey.Items
        monkey.Items <- Seq.empty

    monkeys

let solve (input: string) : string =
    input
    |> String.trimEnd
    |> String.split "\n\n"
    |> Array.map (
        String.split "\n"
        >> Array.map (String.trimStart >> String.split " ")
        >> parseMonkey
    )
    |> Function.iterate 10000 simulateRound
    |> Seq.map (fun monkey -> int64 monkey.Inspections)
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.fold (*) (int64 1)
    |> string
