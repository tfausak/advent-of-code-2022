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
        Assert.AreEqual("72718", Day01.Part1.solve input)

    [<TestMethod>]
    member this.TestDay01Part2() =
        let input = getInput 1
        Assert.AreEqual("213089", Day01.Part2.solve input)

    [<TestMethod>]
    member this.TestDay02Part1() =
        let input = getInput 2
        Assert.AreEqual("11475", Day02.Part1.solve input)

    [<TestMethod>]
    member this.TestDay02Part2() =
        let input = getInput 2
        Assert.AreEqual("16862", Day02.Part2.solve input)

    [<TestMethod>]
    member this.TestDay03Part1() =
        let input = getInput 3
        Assert.AreEqual("8401", Day03.Part1.solve input)

    [<TestMethod>]
    member this.TestDay03Part2() =
        let input = getInput 3
        Assert.AreEqual("2641", Day03.Part2.solve input)
