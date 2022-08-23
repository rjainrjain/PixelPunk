open Parser
open ProjectParser
open ProjectInterpreter
open System
open System.IO

[<EntryPoint>]
let main argv =
  if argv.Length = 0
  then printfn "Usage: dotnet run <file.pixel>";1
  else
  let path = argv.[0]
  let input = File.ReadAllText(path)
  let asto = parse input
  match asto with
  | Some ast -> printfn("Success! Open the generated PixelPunk.svg file for your PixelPunk"); (eval ast)
  | None -> printfn "Invalid program."
  0
