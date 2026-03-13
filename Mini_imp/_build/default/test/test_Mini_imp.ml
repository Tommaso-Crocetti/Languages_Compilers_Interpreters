open Mini_imp_Lib

let programs_dir = "tests"

let test_inputs = [0; 1; 2; 3; 5; 10; -1; -5]

let expected_outputs = [
  ("01_identity.mimp", [0; 1; 2; 3; 5; 10; -1; -5]);
  ("02_constant_zero.mimp", [0; 0; 0; 0; 0; 0; 0; 0]);
  ("03_plus_one.mimp", [1; 2; 3; 4; 6; 11; 0; -4]);
  ("04_minus_one.mimp", [-1; 0; 1; 2; 4; 9; -2; -6]);
  ("05_double.mimp", [0; 2; 4; 6; 10; 20; -2; -10]);
  ("06_square.mimp", [0; 1; 4; 9; 25; 100; 1; 25]);
  ("07_abs.mimp", [0; 1; 2; 3; 5; 10; 1; 5]);
  ("08_clamp_zero.mimp", [0; 1; 2; 3; 5; 10; 0; 0]);
  ("09_of_bool_positive.mimp", [0; 1; 1; 1; 1; 1; 0; 0]);
  ("10_non_negative_flag.mimp", [1; 1; 1; 1; 1; 1; 0; 0]);
  ("11_and_range.mimp", [0; 1; 1; 1; 1; 0; 0; 0]);
  ("12_or_range.mimp", [0; 0; 0; 0; 1; 1; 0; 1]);
  ("13_nested_if.mimp", [0; 1; 1; 1; 1; 1; -1; -1]);
  ("14_seq_assign.mimp", [6; 9; 12; 15; 21; 36; 3; -9]);
  ("15_parenthesized_cmd.mimp", [-1; 0; 3; 8; 24; 99; 0; 24]);
  ("16_sum_to_n.mimp", [0; 1; 3; 6; 15; 55; 0; 0]);
  ("17_factorial.mimp", [1; 1; 2; 6; 120; 3628800; 1; 1]);
  ("18_power_of_two.mimp", [1; 2; 4; 8; 32; 1024; 0; 0]);
  ("19_countdown_steps.mimp", [0; 1; 2; 3; 5; 10; 0; 0]);
  ("20_times_three_loop.mimp", [0; 3; 6; 9; 15; 30; 3; 15]);
  ("21_triangular_shifted.mimp", [3; 6; 10; 15; 28; 78; 1; 0]);
  ("22_fibonacci_iter.mimp", [0; 1; 1; 2; 5; 55; 0; 0]);
  ("23_gcd_subtractive.mimp", [1; 1; 1; 1; 1; 1; 1; 1]);
  ("24_min_with_five.mimp", [0; 1; 2; 3; 5; 5; -1; -5]);
  ("25_max_with_zero.mimp", [0; 1; 2; 3; 5; 10; 0; 0]);
  ("26_even_flag.mimp", [1; 0; 1; 0; 0; 1; 0; 0]);
  ("27_sign.mimp", [0; 1; 1; 1; 1; 1; -1; -1]);
  ("28_skip_then_assign.mimp", [1; 2; 5; 10; 26; 101; 2; 26]);
  ("29_complex_expr.mimp", [-6; -4; 0; 6; 24; 104; -7; 13]);
  ("30_while_to_zero.mimp", [0; 1; 2; 3; 5; 10; 0; 0]);
]

let parse_file path =
  let ic = open_in path in
  let lexbuf = Lexing.from_channel ic in
  let prog = Mini_imp_Parser.prg Mini_imp_Lexer.read lexbuf in
  close_in ic;
  prog

let is_mimp_file name = Filename.check_suffix name ".mimp"

let sorted_program_files dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.filter is_mimp_file
  |> List.sort String.compare

let expected_for_file fname = List.assoc_opt fname expected_outputs

let run_program_file dir fname =
  let path = Filename.concat dir fname in
  Printf.printf "=== %s ===\n%!" fname;
  let counts =
    try
      let prog = parse_file path in
      (match expected_for_file fname with
      | None ->
          Printf.printf "  [NO EXPECTED] syntax check only (parsed successfully)\n%!";
          (1, 1)
      | Some expected ->
          if List.length expected <> List.length test_inputs then (
            Printf.printf "  [EXPECTED ERROR] expected vector length mismatch\n%!";
            (1, 0)
          ) else (
            let all_ok = ref true in
            List.iter2
              (fun n exp ->
                try
                  let result = Mini_imp.execute prog n in
                  Printf.printf "  f(%3d) = %d\n%!" n result;
                  if result <> exp then (
                    all_ok := false;
                    Printf.printf "    [MISMATCH] expected %d, got %d\n%!" exp result
                  )
                with e ->
                  all_ok := false;
                  Printf.printf "  f(%3d) raised: %s\n%!" n (Printexc.to_string e))
              test_inputs
              expected;
            if !all_ok then (1, 1) else (1, 0)
          ))
    with e ->
      Printf.printf "  [PARSE ERROR] %s\n%!" (Printexc.to_string e);
      (0, 0)
  in
  print_newline ();
  counts

let () =
  if not (Sys.file_exists programs_dir && Sys.is_directory programs_dir) then
    failwith
      "Cannot locate test/tests. Ensure the tests directory exists inside Mini_imp/test.";

  let files = sorted_program_files programs_dir in
  Printf.printf "Found %d test programs in %s\n\n%!" (List.length files) programs_dir;
  let parsed_count = ref 0 in
  let executed_count = ref 0 in
  List.iter
    (fun f ->
      let p, e = run_program_file programs_dir f in
      parsed_count := !parsed_count + p;
      executed_count := !executed_count + e)
    files;
  Printf.printf
    "Summary: parsed %d/%d, correct %d/%d\n%!"
    !parsed_count
    (List.length files)
    !executed_count
    (List.length files);
  if !parsed_count <> List.length files || !executed_count <> List.length files then
    failwith "One or more Mini_Imp tests failed"
