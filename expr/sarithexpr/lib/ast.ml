exception TypeError of string

type expr =
    True
  | False
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | If of expr * expr * expr
  | Zero
  | Succ of expr
  | Pred of expr
  | IsZero of expr

type exprval =
  | Bool of bool
  | Nat of int

type exprtype = 
  | BoolT
  | NatT


let rec string_of_expr = function
  | True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Not e -> "Not(" ^ (string_of_expr e) ^ ")"
  | And(e1, e2) -> "And(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Or(e1, e2) -> "Or(" ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Zero -> "0"
  | Pred(e) -> "Pred(" ^ (string_of_expr e) ^ ")"
  | Succ(e) -> "Succ(" ^ (string_of_expr e) ^ ")"
  | IsZero(e) -> "IsZero(" ^ (string_of_expr e) ^ ")"


let string_of_val = function
  | Bool b -> string_of_bool b
  | Nat n -> string_of_int n


let string_of_type = function
  | BoolT -> "Bool"
  | NatT -> "Nat"


let rec is_nv = function
  | Zero -> true
  | Succ e -> is_nv(e) 
  | _ -> false


let raise_type_error current expect expr =
  raise (TypeError ((string_of_expr expr) ^ " has type " ^ current ^ ", but type " ^ expect ^ " was expected."))