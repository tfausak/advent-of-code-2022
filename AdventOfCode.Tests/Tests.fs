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

    [<TestMethod>]
    member this.TestDay06Part2() = test 6 2 "3708"

    [<TestMethod>]
    member this.TestDay07Part1() = test 7 1 "1778099"

    [<TestMethod>]
    member this.TestDay07Part2() = test 7 2 "1623571"

    [<TestMethod>]
    member this.TestDay08Part1() = test 8 1 "1803"

    [<TestMethod>]
    member this.TestDay08Part2() = test 8 2 "268912"

    [<TestMethod>]
    member this.TestDay09Part1() = test 9 1 "5960"

    [<TestMethod>]
    member this.TestDay09Part2() = test 9 2 "2327"

    [<TestMethod>]
    member this.TestDay10Part1() = test 10 1 "14220"

    [<TestMethod>]
    member this.TestDay10Part2() = test 10 2 "ZRARLFZU"

    [<TestMethod>]
    member this.TestDay11Part1() = test 11 1 "76728"

    [<TestMethod>]
    member this.TestDay11Part2() = test 11 2 "21553910156"

    [<TestMethod>]
    member this.TestDay12Part1() = test 12 1 "462"

    [<TestMethod>]
    member this.TestDay12Part2() = test 12 2 "451"
