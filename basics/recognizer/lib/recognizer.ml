let rec lang1 list = match list with
  | [x] when (x == '0') || (x == '1') -> true
  | x::list when (x == '0') || (x == '1') -> true && lang1 list
  | _ -> false
;;

let rec lang2_rec list = match list with
  | [] -> true
  | [x] when (x == '0') || (x == '1') -> true
  | '1'::list -> true && lang2_rec list
  | _ -> false
;;

let lang2 list = lang2_rec(List.rev(list));;

let lang3 _ = failwith ""

let lang4 _ = failwith ""

let lang5 _ = failwith ""
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
