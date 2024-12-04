module T = ANSITerminal
open Life.Main

let rule_s23_b3 = { Life.Main.survive = [2; 3]; Life.Main.birth = [3] }

let _ =
  match Array.length Sys.argv with
  | 3 ->
      let rule_str = Sys.argv.(1) in
      let rounds = int_of_string (Sys.argv.(2)) in
      let rule = parse_rule rule_str in
      T.erase T.Screen;
      T.save_cursor ();
      Random.self_init ();
      let w = loop init_w rounds rule in
      display w;
      ignore (read_line ());
      T.restore_cursor ();
      print_newline ()
  | _ -> failwith "Usage: dune exec life S/B_rule n_rounds"