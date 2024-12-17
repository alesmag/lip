open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies


let rec trace1 = function
  | If(True, e1, _) -> e1
  | If(False, _, e2) -> e2
  | If(e1, e2, e3) -> If(trace1 e1, e2, e3)
  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> Not(trace1 e)
  | And(True, e2) -> e2
  | And(False, _) -> False
  | And(e1, e2) -> And(trace1 e1, e2)
  | Or(True, _) -> True
  | Or(False, e2) -> e2
  | Or(e1, e2) -> Or(trace1 e1, e2)
  | Succ(e) -> Succ(trace1 e)
  | Pred(Succ(n)) when (is_nv(n)) -> n
  | Pred(e) -> Pred(trace1 e)
  | IsZero(Zero) -> True
  | IsZero(Succ(_)) -> False
  | IsZero(e) -> IsZero(trace1 e)  
  | _ -> raise NoRuleApplies


let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


let rec eval : expr -> exprval = function
  | True -> Bool true
  | False -> Bool false
  | Zero -> Nat 0
  | If(e1,e2,e3) -> (
      match eval e1 with
      | Bool true -> eval e2
      | Bool false -> eval e3
      | _ -> failwith "Error If"
    )
  | Not e -> (
      match eval e with
      | Bool b -> Bool (not b)
      | _ -> failwith "Error Not"
    )
  | And(e1, e2) -> (
      match eval e1 with
      | Bool true -> eval e2
      | Bool false -> Bool false
      | _ -> failwith "Error And"
    )
  | Or(e1, e2) -> (
      match eval e1 with
      | Bool true -> Bool true
      | Bool false -> eval e2
      | _ -> failwith "Error Or"
    )
  | Succ(e) -> (
      match eval e with 
      | Nat n -> Nat (n + 1)
      | _ -> failwith "Error Succ"
    ) 
  | Pred(e) -> (
      match eval e with 
      | Nat n when (n > 0) -> Nat (n - 1)
      | _ -> failwith "Error Pred"
    )
  | IsZero(e) -> (
      match eval e with
      | Nat 0 -> Bool true
      | Nat _ -> Bool false
      | _ -> failwith "Error IsZero"
    )


let rec typecheck (e : expr) : exprtype =
  match e with
  | True -> BoolT
  | False -> BoolT
  | Zero -> NatT
  | Succ(e) | Pred(e) -> (
    match typecheck e with
    | NatT -> NatT
    | BoolT -> raise_type_error "Bool" "Nat" e
    )
  | IsZero(e) -> (
    match typecheck e with
    | NatT -> BoolT
    | BoolT -> raise_type_error "Bool" "Nat" e
    )
  | Not(e) -> (
    match typecheck e with
    | BoolT -> BoolT
    | NatT -> raise_type_error "Nat" "Bool" e
    )
  | And(e1, e2) | Or(e1, e2) -> (
    let t1 = typecheck e1 in
    let t2 = typecheck e2 in
    match t1, t2 with
    | BoolT, BoolT -> BoolT
    | BoolT, NatT -> raise_type_error "Nat" "Bool" e2
    | NatT, BoolT -> raise_type_error "Nat" "Bool" e1
    | NatT, NatT -> raise_type_error "Nat" "Bool" e
    )
  | If(e1, e2, e3) -> (
    let t1 = typecheck e1 in
    let t2 = typecheck e2 in
    let t3 = typecheck e3 in
    match t1 with
    | BoolT ->
      if t2 = t3 
        then t2
      else 
        raise (TypeError ("Branches of If have different types"))
    | NatT -> raise_type_error "Nat" "Bool" e1
    )

