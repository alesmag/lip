open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)
let rec take n l =
  match l with
  | [] -> []
  | x :: xs -> if n > 0 then x :: take (n - 1) xs else []
;;

let frequency n l =
  (* Associa un indice agli elementi per preservare l'ordine di lettura *)
  let indexed_list = List.mapi (fun i x -> (x, i)) l in
  let frequencies =
    List.fold_left (fun acc (x, i) ->
      match List.assoc_opt x acc with
      | Some (count, first_index) ->
          (x, (count + 1, first_index)) :: List.remove_assoc x acc
      | None ->
          (x, (1, i)) :: acc
    ) [] indexed_list
  in
  (* Ordina per frequenza decrescente *)
  let sorted_frequencies =
    List.sort (fun (_, (f1, i1)) (_, (f2, i2)) ->
      if f1 = f2 then compare i1 i2 else compare f2 f1
    ) frequencies
  in
  (* Rimuovi gli indici e prendi i primi n elementi *)
  take n (List.map (fun (x, (freq, _)) -> (x, freq)) sorted_frequencies)
;;