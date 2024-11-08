let rec lang1 list = 
  match list with
  | [x] when (x = '0') || (x = '1') -> true
  | x::list when (x = '0') || (x = '1') -> true && lang1 list
  | _ -> false
;;

let rec lang2_rec list = 
  match list with
  | [] -> true
  | [x] when (x = '0') || (x = '1') -> true
  | '1'::list -> true && lang2_rec list
  | _ -> false
;;

let lang2 list = 
  lang2_rec(List.rev(list))
;;

let rec lang3_rec list =
  match list with
  | ['0'] -> true
  | x::lst when (x = '0') || (x = '1') -> lang3_rec lst 
  | _ -> false
;; 

let lang3 list =
  match list with 
  | '0'::lst -> lang3_rec lst 
  | _ -> false
;;

let rec lang4_rec2 list =
  match list with
  | [] -> true
  | '0'::lst -> lang4_rec2 lst
  | _ -> false
;;

let rec lang4_rec1 list =
  match list with
  | '0'::lst -> lang4_rec1 lst
  | '1'::lst -> lang4_rec2 lst
  | _ -> false 
;; 

let rec lang4 list =
  match list with
  | '0'::lst -> lang4 lst
  | '1'::lst -> lang4_rec1 lst
  | _ -> false
;;

let rec lang5_rec list =
  match list with
  | [] -> true
  | '0'::'0'::lst -> lang5_rec lst
  | '1'::'1'::lst -> lang5_rec lst
  | _ -> false
;;

let lang5 list =
  match list with
  | '0'::'0'::lst -> lang5_rec lst
  | '1'::'1'::lst -> lang5_rec lst
  | _ -> false
;;
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers