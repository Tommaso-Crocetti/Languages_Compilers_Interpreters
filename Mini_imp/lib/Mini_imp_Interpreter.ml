let run_program (program : Mini_imp.program) =
  print_endline "=== Input ===";
  let input = read_int () in
  print_endline "=== Output ===";
  print_int (Mini_imp.execute program input);
  print_newline ()
