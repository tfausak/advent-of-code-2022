namespace AdventOfCode.Tests

open AdventOfCode.Library
open Microsoft.VisualStudio.TestTools.UnitTesting
open System.IO

[<TestClass>]
type TestClass() =
    let getInput (day: int) : string =
        File.ReadAllText $"../../../../AdventOfCode.Library/Day%02d{day}/input.txt"

    [<TestMethod>]
    member this.TestDay01Part1() =
        let input = getInput 1
        Assert.AreEqual(Some "72718", Solver.solve 1 1 input)

    [<TestMethod>]
    member this.TestDay01Part2() =
        let input = getInput 1
        Assert.AreEqual(Some "213089", Solver.solve 1 2 input)

    [<TestMethod>]
    member this.TestDay02Part1() =
        let input = getInput 2
        Assert.AreEqual(Some "11475", Solver.solve 2 1 input)

    [<TestMethod>]
    member this.TestDay02Part2() =
        let input = getInput 2
        Assert.AreEqual(Some "16862", Solver.solve 2 2 input)

    [<TestMethod>]
    member this.TestDay03Part1() =
        let input = getInput 3
        Assert.AreEqual(Some "8401", Solver.solve 3 1 input)

    [<TestMethod>]
    member this.TestDay03Part2() =
        let input = getInput 3
        Assert.AreEqual(Some "2641", Solver.solve 3 2 input)

    [<TestMethod>]
    member this.TestDay04Part1() =
        let input = getInput 4
        Assert.AreEqual(Some "466", Solver.solve 4 1 input)

    [<TestMethod>]
    member this.TestDay04Part2() =
        let input = getInput 4
        Assert.AreEqual(Some "865", Solver.solve 4 2 input)
