module ProjectInterpreter

open System.IO
open ProjectParser

(*
 * Generates svg code from the PixelPunk AST provided
 *
 * @param e The PixelPunk expression.
 * @return A string of svg code enacting the PixelPunk AST
 *)
let rec evalHelper e : string =
  match e with
  | Color c -> "#" + c
  | Pixel(x, y, color) ->
    "<rect fill=\"" + evalHelper(color) + "\" x=\"" + string(x) + "\" y=\"" + string(y) + "\" width=\"1\" height=\"1\" />"
  | Attribute xs -> 
    let strlist = xs |> List.map (evalHelper)
    List.fold (fun x y -> x + "\n" + y) "" strlist

(*
 * Writes svg code from evalHelper to a file
 *
 * @param expr The PixelPunk expression.
 * @return Nothing.
 *)
let eval expr =
  let path = "./PixelPunk.svg"
  let svgCodeStr = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\">" + evalHelper(expr) + "\n</svg>"
  //use file = File.Create(path)
  File.WriteAllText(path, svgCodeStr)
 
