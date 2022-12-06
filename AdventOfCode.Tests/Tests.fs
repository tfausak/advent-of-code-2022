namespace AdventOfCode.Tests

open AdventOfCode.Library
open Microsoft.VisualStudio.TestTools.UnitTesting
open System.IO

[<TestClass>]
type TestClass() =
    let getInput (day: int) : string =
        File.ReadAllText $"../../../../AdventOfCode.Library/Day%02d{day}/input.txt"

    let test (day: int) (part: int) (expected: string) : unit =
        let input = getInput day
        Assert.AreEqual(Some expected, Solver.solve day part input)

    [<TestMethod>]
    member this.TestDay01Part1() = test 1 1 "72718"

    [<TestMethod>]
    member this.TestDay01Part2() = test 1 2 "213089"

    [<TestMethod>]
    member this.TestDay02Part1() = test 2 1 "11475"

    [<TestMethod>]
    member this.TestDay02Part2() = test 2 2 "16862"

    [<TestMethod>]
    member this.TestDay03Part1() = test 3 1 "8401"

    [<TestMethod>]
    member this.TestDay03Part2() = test 3 2 "2641"

    [<TestMethod>]
    member this.TestDay04Part1() = test 4 1 "466"

    [<TestMethod>]
    member this.TestDay04Part2() = test 4 2 "865"

    [<TestMethod>]
    member this.TestDay05Part1() = test 5 1 "VRWBSFZWM"

    [<TestMethod>]
    member this.TestDay05Part2() = test 5 2 "RBTWJWMCF"

    [<TestMethod>]
    member this.TestDay06Part1() = test 6 1 "1723"
