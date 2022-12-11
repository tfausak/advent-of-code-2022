module AdventOfCode.Library.Day11.Part1

open AdventOfCode.Library.Common

type Operation =
    | Add
    | Multiply

type Operand =
    | Old
    | Value of int

type Monkey =
    { mutable items: int seq
      operation: Operation
      operand: Operand
      divisor: int
      truthy: int
      falsey: int
      mutable inspections: int }

let parseMonkey (ls: string array array) : Monkey =
    { items = ls[1] |> Array.skip 2 |> Seq.map (String.filter (fun c -> c <> ',') >> int)
      operation =
        match ls[2].[4] with
        | "+" -> Add
        | "*" -> Multiply
        | x -> failwith $"invalid operation: %A{x}"
      operand =
        match ls[2].[5] with
        | "old" -> Old
        | n -> Value(int n)
      divisor = int ls[3].[3]
      truthy = int ls[4].[5]
      falsey = int ls[5].[5]
      inspections = 0 }

let divisibleBy (d: int) (n: int) : bool = n % d = 0

let simulateRound (monkeys: Monkey array) : Monkey array =
    for monkey in monkeys do
        for l1 in monkey.items do
            let n =
                match monkey.operand with
                | Old -> l1
                | Value x -> x

            let l2 =
                match monkey.operation with
                | Add -> l1 + n
                | Multiply -> l1 * n

            let l3 = l2 / 3

            let i =
                if divisibleBy monkey.divisor l3 then
                    monkey.truthy
                else
                    monkey.falsey

            monkeys[i].items <- Seq.append monkeys[i].items (Seq.singleton l3)

        monkey.inspections <- monkey.inspections + Seq.length monkey.items
        monkey.items <- Seq.empty

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
    |> Seq.map (fun monkey -> monkey.inspections)
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.fold (*) 1
    |> string
