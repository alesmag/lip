(* tokens *)
type token = A | B | X

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
  
(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)
         
let toklist_of_string s =
  let char_list = explode s in
  List.filter_map
  (
    function
    | 'A' -> Some A
    | 'B' -> Some B
    | '=' -> Some X
    | _ -> None
  ) char_list
;;


(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)

let rec valid_rec list expected =
  match list, expected with
  | [], _ -> true
  | A::lst, A -> valid_rec lst A
  | X::lst, A | X::lst, X -> valid_rec lst X
  | B::lst, X | B::lst, B -> valid_rec lst B
  | _ -> false
;;

let valid l = 
  valid_rec l A
;;


(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)
let count list =
  let count_a = List.length(List.filter(fun x -> x = A) list) in
  let count_b = List.length(List.filter(fun x -> x = B) list) in
  count_a, count_b
;;

let win l = 
  let count_a, count_b = count l in
  if (count_a > count_b) then A
  else if (count_b > count_a) then B
  else X
;;


(* val string_of_winner : token -> string *)
let string_of_winner w = 
  match w with
  | A -> "Vince la squadra A"
  | B -> "Vince la squadra B"
  | X -> "Parità"