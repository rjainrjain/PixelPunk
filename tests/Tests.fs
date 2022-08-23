namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open ProjectParser
open ProjectInterpreter
open System.IO


[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.ValidProgramReturnsValidSVG() =
      let input = "PixelPunk<nose=Attribute{Pixel(row=1,col=1,color=#FFFFFF)}nose>"
      let expected = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\">\n<rect fill=\"#FFFFFF\" x=\"0\" y=\"0\" width=\"1\" height=\"1\" />\n</svg>"
      let tree = parse input
      let nothing = eval tree.Value
      let svg = File.ReadAllText("PixelPunk.svg")
      Assert.AreEqual(expected,svg)
    [<TestMethod>]
    member this.testParser() =
      let input = "PixelPunk<nose=Attribute{Pixel(row=1,col=1,color=#FFFFFF)}nose>"
      let expected = Sequence([Assignment(Variable("nose"), Attribute([Pixel(1,1,Color("FFFFFF"))]));Variable("nose")])
      let treeoption = parse input
      let tree = treeoption.Value
      Assert.AreEqual(expected, tree)
    [<TestMethod>]
    member this.testEval() =
      let tree = Sequence([Assignment(Variable("nose"), Attribute([Pixel(1,1,Color("FFFFFF"))]));Variable("nose")])
      let expected = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\">\n<rect fill=\"#FFFFFF\" x=\"0\" y=\"0\" width=\"1\" height=\"1\" />\n</svg>"
      let nothing = eval tree
      let svg = File.ReadAllText("PixelPunk.svg")
      Assert.AreEqual(expected,svg)
    
