open Boolexpr.Main

let%test "test_eval_1" = parse "false" |> eval = false
let%test "test_eval_2" = parse "true" |> eval = true
let%test "test_eval_3" = parse "if true then false else true" |> eval = false
let%test "test_eval_4" = parse "if false then false else true" |> eval = true
let%test "test_eval_5" = parse "if true then (if true then false else true) else (if true then true else false)" |> eval = false
let%test "test_eval_6" = parse "if (if false then false else false) then (if false then true else false) else (if true then false else true)" |> eval = false
let%test "test_eval_7" = parse "if (if (if false then false else false) then (if false then true else false) else (if true then false else true)) then (if false then true else false) else (if true then false else true)" |> eval = false