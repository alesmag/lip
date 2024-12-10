open Ast

let rec string_of_boolexpr = function
  | True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"


let parse (s : string) : boolExpr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies

let rec trace1 = function
  | If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e1,e2,e3) -> 
    let e1' = trace1 e1 in
    If(e1', e2, e3)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


let eval = function
  | True -> true
  | False -> false
  | If(e1,e2,e3) -> if is_value e1 then is_value e2 else is_value e3
