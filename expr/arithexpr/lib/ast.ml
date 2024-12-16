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

let string_of_val = function
  | Bool b -> string_of_bool b
  | Nat n -> string_of_int n

let rec is_nv = function
  | Zero -> true
  | Succ e -> is_nv(e) 
  | _ -> false