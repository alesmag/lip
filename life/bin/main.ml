module T = ANSITerminal
open Life.Main

let rule_s23_b3 = { Life.Main.survive = [2; 3]; Life.Main.birth = [3] }

let _ = match Array.length(Sys.argv) with
    2 -> let k = int_of_string (Sys.argv.(1)) in
    T.erase T.Screen;
    T.save_cursor();
    Random.self_init();
    let w = loop init_w k rule_s23_b3 in
    display w;
    ignore(read_line());
    T.restore_cursor();
    print_newline()
  (* wrong usage *)
  | _ -> failwith "Usage: dune exec life n_rounds"
