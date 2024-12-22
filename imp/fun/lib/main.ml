open Ast
open Types

let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies

let botenv = fun x -> failwith ("variable " ^ x ^ " unbound")
let botmem = fun l -> failwith ("location " ^ string_of_int l ^ " undefined")
let bind f x v = fun y -> if y = x then v else f y

let apply st x = 
  match topenv st x with
  | IVar l
  | BVar l -> getmem st l


let rec eval_expr st = function
  | True -> Bool true
  | False -> Bool false
  | Var x -> apply st x
  | Const n -> Int n

  | Not(e) -> (
    match eval_expr st e with
    | Bool b -> Bool(not b)
    | _ -> raise (TypeError "Not")
    )
  
  | And(e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Bool b1, Bool b2 -> Bool(b1 && b2)
    | _ -> raise (TypeError "And")
    )
  
  | Or(e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Bool b1, Bool b2 -> Bool(b1 || b2)
    | _ -> raise (TypeError "Or")
    )

  | Add(e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Int n1, Int n2 -> Int(n1 + n2)
    | _ -> raise (TypeError "Add")
    )
  
  | Sub(e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Int n1, Int n2 when (n1 >= n2) -> Int(n1 - n2)
    | _ -> raise (TypeError "Sub")
    )
  
  | Mul(e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Int n1, Int n2 -> Int(n1 * n2)
    | _ -> raise (TypeError "Mul")
    )

  | Eq(e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Int n1, Int n2 -> Bool(n1 = n2)
    | _ -> raise (TypeError "Eq")
    )

  | Leq(e1, e2) -> (
    match eval_expr st e1, eval_expr st e2 with
    | Int n1, Int n2 -> Bool(n1 <= n2)
    | _ -> raise (TypeError "Leq")
    )


let rec eval_decl (e, l) decls =
  match decls with
  | [] -> (e, l)
  | IntVar(x) :: rest -> 
      let e' = bind e x (IVar l) in 
      eval_decl (e', l+1) rest
  | BoolVar(x) :: rest -> 
      let e' = bind e x (BVar l) in
      eval_decl (e', l+1) rest


let rec trace1 = function
  | St _ -> raise NoRuleApplies
  | Cmd(c, st) -> (
    match c with
    | Skip -> St st
    | Assign(x, e) -> (
      match eval_expr st e, topenv st x with
      | Bool v, BVar l -> St (setmem st (bind (getmem st) l (Bool v)))
      | Int v, IVar l -> St (setmem st (bind (getmem st) l (Int v)))
      | _ -> failwith "Error Assign"
      )
    | Seq(c1, c2) -> (
      match trace1 (Cmd(c1, st)) with
      | St st1 -> Cmd(c2, st1)
      | Cmd(c1', st1) -> Cmd(Seq(c1', c2), st1)
      )
    | If(e, c1, c2) -> (
      match eval_expr st e with
      | Bool true -> Cmd(c1, st)
      | Bool false -> Cmd(c2, st)
      | _ -> raise (TypeError "If")
      )
    | While(e, c) -> (
      match eval_expr st e with
      | Bool true -> Cmd(Seq(c, While(e, c)), st)
      | Bool false -> St st
      | _ -> raise (TypeError "While")
      )
    | Decl(d, c) -> (
      let (e, l) = eval_decl (topenv st, getloc st) d in
      let st' = make_state (e::(getenv st)) (getmem st) l in
      Cmd(Block(c), st')
      )
    | Block(c) -> (
      match trace1 (Cmd(c, st)) with
      | St st -> St (setenv st (popenv st))
      | Cmd(c1', st1) -> Cmd(Block(c1'), st1)
      )
    )


let rec trace_rec n t =
  if n <= 0 
    then [t]
  else 
    try
      let t' = trace1 t in 
      t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t]


let trace n c = trace_rec n (Cmd(c, make_state [botenv] botmem 0))
