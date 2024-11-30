open Toylexer.Token
open Toylexer.Main

let%test "test_frequencies_1" =
  lexer "x=y; x=x+1" |> frequency 3 = [(CTOK "x", 3); (ASSIGN, 2); (CTOK "y", 1)]

(* YOUR TESTS HERE *)

let%test "test_atok" =
  lexer "Ciao a tutti" |> frequency 3 = [(ATOK "Ciao", 1); (BTOK "a", 1); (ID "tutti", 1)]

let%test "test_btok" =
  lexer "a ieaei a o" |> frequency 3 = [(BTOK "a", 2); (BTOK "ieaei", 1); (BTOK "o", 1)]

let%test "test_ctok" =
  lexer "figaro lally chap" |> frequency 3 = [(ID "figaro", 1); (CTOK "lally", 1); (CTOK "chap", 1)]

let%test "test_dtok" =
  lexer "-3.14 7 94." |> frequency 3 = [(DTOK "-3.14", 1); (DTOK "7", 1); (DTOK "94.", 1)]

let%test "test_etok" =
  lexer "0x34 0X00 0x5D" |> frequency 3 = [(ETOK "0x34", 1); (ETOK "0X00", 1); (ETOK "0x5D", 1)]
