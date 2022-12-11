module AdventOfCode.Library.Day11.Part1

open AdventOfCode.Library.Common

type Operation =
    | Add
    | Multiply

type Operand =
    | Old
    | Value of int

type Monkey =
    { mutable Items: int seq
      Operation: Operation
      Operand: Operand
      Divisor: int
      Truthy: int
      Falsey: int
      mutable Inspections: int }

let parseMonkey (ls: string array array) : Monkey =
    { Items = ls[1] |> Array.skip 2 |> Seq.map (String.filter (fun c -> c <> ',') >> int)
      Operation =
        match ls[2].[4] with
        | "+" -> Add
        | "*" -> Multiply
        | x -> failwith $"invalid Operation: %A{x}"
      Operand =
        match ls[2].[5] with
        | "old" -> Old
        | n -> Value(int n)
      Divisor = int ls[3].[3]
      Truthy = int ls[4].[5]
      Falsey = int ls[5].[5]
      Inspections = 0 }

let divisibleBy (d: int) (n: int) : bool = n % d = 0

let simulateRound (monkeys: Monkey array) : Monkey array =
    for monkey in monkeys do
        for l1 in monkey.Items do
            let n =
                match monkey.Operand with
                | Old -> l1
                | Value x -> x

            let l2 =
                match monkey.Operation with
                | Add -> l1 + n
                | Multiply -> l1 * n

            let l3 = l2 / 3

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
    |> Function.iterate 20 simulateRound
    |> Seq.map (fun monkey -> monkey.Inspections)
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.fold (*) 1
    |> string
