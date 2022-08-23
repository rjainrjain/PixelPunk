module ProjectInterpreter

open System.IO
open ProjectParser

(* environment to store variables and values *)
type Env = Map<string,string>

(*
 * Generates svg code from the PixelPunk AST provided
 *
 * @param e The PixelPunk expression.
 * @return A string of svg code enacting the PixelPunk AST
 *)
let rec evalHelper (e: PixelPunk) (env: Env) : string * Env =
  match e with
  | Color c -> (("#" + c), env)
  | Pixel(row, col, color) ->
        let colorval = fst(evalHelper color env)
        (("<rect fill=\"" + colorval + "\" x=\"" + string(col - 1) + "\" y=\"" + string(row - 1) + "\" width=\"1\" height=\"1\" />"),env)
  | Attribute xs -> 
      let strlist = xs |> List.map (fun x -> fst(evalHelper x env))
      ((List.fold (fun x y -> x + "\n" + y) "" strlist),env)
  | Sequence ys -> 
        match ys with
        | [] -> "", env 
        | (Assignment (v, p))::ys' ->
            let s1,env' = evalHelper ys.Head env
            let s2,env'' = evalHelper (Sequence ys') env'
            ((s1 + s2), env'')
        | a::ys' -> 
            let s2,env' = evalHelper (Sequence ys') env
            ((fst(evalHelper ys.Head env) + s2), env')
  | Canvas c -> 
      let colorval = fst(evalHelper c env) 
      (("<rect fill=\"" + colorval + "\" x=\"0\" y=\"0\" width=\"24\" height=\"24\" />"),env)     
  | Variable x ->
        if env.ContainsKey(x) then (env.Item(x),env)
        else failwith("No variable defined as '" + x + "'")
  | Assignment (var,expr) ->
        match var with
        | Variable x ->
            let expr',env' = evalHelper expr env
            let env'' = env'.Add(x,expr')
            ("",env'')
        | _ -> failwith("Expression before assignment operator must be a variable")

(*
 * Writes svg code from evalHelper to a file
 *
 * @param expr The PixelPunk expression.
 * @return Nothing.
 *)
let eval expr =
  let path = "./PixelPunk.svg"
  let svgCode,map = evalHelper expr Map.empty
  let svgCodeStr = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\">" + svgCode + "\n</svg>"
  //use file = File.Create(path)
  File.WriteAllText(path, svgCodeStr)
 
