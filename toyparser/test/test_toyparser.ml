open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = Ok 9

(* YOUR TESTS HERE *)
let%test "test_eval_2" = parse "3 + (3 + 2)" |> eval = Ok 8

let%test "test_eval_3" = parse "(1 + 2) + 3 + (1 + 2)" |> eval = Ok 9

let%test "test_eval_4" = parse "(7 + 2)" |> eval = Ok 9

let%test "test_eval_5" = parse "1" |> eval = Ok 1

let%test "test_eval_6" = parse "2 * 2 / 2" |> eval = Ok 2

let%test "test_eval_7" = parse "(2 + (3 * 2) / 2) / 5" |> eval = Ok 1

let%test "test_eval_8" = parse "-1 - 2 - -3" |> eval = Ok 0

let%test "test_eval_9" = parse "-5 / -1" |> eval = Ok 5

let%test "test_eval_10" = parse "(5 * 2) / -2" |> eval = Ok (-5)

let%test "test_eval_11" = parse "0x01 + 2" |> eval = Ok 3

let%test "test_eval_12" = parse "0x1A + 5" |> eval = Ok 31

let%test "test_eval_13" = parse "(0x0F * 2) / 0x05" |> eval = Ok 6

let%test "test_eval_14" = parse "(0x0F * 2) / 0x00" |> eval = Error(Printf.sprintf("Error: can't divide 30 by zero"))