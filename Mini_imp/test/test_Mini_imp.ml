open Mini_imp_Lib

let programs_dir = "tests"

let test_inputs = [0; 1; 2; 3; 5; 10; -1; -5]

let string_of_reg = function
  | Mini_RISC.Rin -> "Rin"
  | Mini_RISC.Rout -> "Rout"
  | Mini_RISC.RVar n -> "R" ^ string_of_int n

let string_of_brop = function
  | Mini_RISC.Add -> "Add"
  | Mini_RISC.Sub -> "Sub"
  | Mini_RISC.Mult -> "Mult"
  | Mini_RISC.And -> "And"
  | Mini_RISC.Less -> "Less"
  | Mini_RISC.Or -> "Or"

let string_of_biop = function
  | Mini_RISC.AddI -> "AddI"
  | Mini_RISC.SubI -> "SubI"
  | Mini_RISC.MultI -> "MultI"
  | Mini_RISC.AndI -> "AndI"
  | Mini_RISC.OrI -> "OrI"

let string_of_urop = function
  | Mini_RISC.Not -> "Not"
  | Mini_RISC.Copy -> "Copy"

let string_of_instruction = function
  | Mini_RISC.Nop -> "Nop"
  | Mini_RISC.Brop (op, left, right, dst) ->
      Printf.sprintf "Brop(%s,%s,%s,%s)"
        (string_of_brop op)
        (string_of_reg left)
        (string_of_reg right)
        (string_of_reg dst)
  | Mini_RISC.Biop (op, src, imm, dst) ->
      Printf.sprintf "Biop(%s,%s,%d,%s)"
        (string_of_biop op)
        (string_of_reg src)
        imm
        (string_of_reg dst)
  | Mini_RISC.Urop (op, src, dst) ->
      Printf.sprintf "Urop(%s,%s,%s)"
        (string_of_urop op)
        (string_of_reg src)
        (string_of_reg dst)
  | Mini_RISC.Load (src, dst) ->
      Printf.sprintf "Load(%s,%s)" (string_of_reg src) (string_of_reg dst)
  | Mini_RISC.LoadI (n, dst) ->
      Printf.sprintf "LoadI(%d,%s)" n (string_of_reg dst)
  | Mini_RISC.Store (src, dst) ->
      Printf.sprintf "Store(%s,%s)" (string_of_reg src) (string_of_reg dst)
  | Mini_RISC.Jump label -> Printf.sprintf "Jump(%s)" label
  | Mini_RISC.CJump (cond, l1, l2) ->
      Printf.sprintf "CJump(%s,%s,%s)" (string_of_reg cond) l1 l2

let string_of_instructions code =
  code
  |> List.map string_of_instruction
  |> String.concat "; "

let normalize_instructions code =
  let next = ref 0 in
  let seen = Hashtbl.create 8 in
  let normalize_reg = function
    | Mini_RISC.Rin -> Mini_RISC.Rin
    | Mini_RISC.Rout -> Mini_RISC.Rout
    | Mini_RISC.RVar n ->
        (match Hashtbl.find_opt seen n with
        | Some mapped -> Mini_RISC.RVar mapped
        | None ->
            incr next;
            Hashtbl.add seen n !next;
            Mini_RISC.RVar !next)
  in
  let normalize_instruction = function
    | Mini_RISC.Nop -> Mini_RISC.Nop
    | Mini_RISC.Brop (op, left, right, dst) ->
      Mini_RISC.Brop (op, normalize_reg left, normalize_reg right, normalize_reg dst)
    | Mini_RISC.Biop (op, src, imm, dst) ->
        Mini_RISC.Biop (op, normalize_reg src, imm, normalize_reg dst)
    | Mini_RISC.Urop (op, src, dst) ->
        Mini_RISC.Urop (op, normalize_reg src, normalize_reg dst)
    | Mini_RISC.Load (src, dst) ->
        Mini_RISC.Load (normalize_reg src, normalize_reg dst)
    | Mini_RISC.LoadI (n, dst) ->
        Mini_RISC.LoadI (n, normalize_reg dst)
    | Mini_RISC.Store (src, dst) ->
        Mini_RISC.Store (normalize_reg src, normalize_reg dst)
    | Mini_RISC.Jump label -> Mini_RISC.Jump label
    | Mini_RISC.CJump (cond, l1, l2) ->
        Mini_RISC.CJump (normalize_reg cond, l1, l2)
  in
  List.map normalize_instruction code

let assert_translation name expr expected =
  let reg_map = Mini_RISC.initial_reg_map "input" "output" in
  let (_, actual) = Mini_RISC.translate_aexpr expr None reg_map in
  let actual = normalize_instructions actual in
  if actual <> expected then
    failwith
      (Printf.sprintf
         "translation test '%s' failed\nexpected: [%s]\nactual:   [%s]"
         name
         (string_of_instructions expected)
         (string_of_instructions actual))

let run_translation_tests () =
  let open Mini_imp in
  let tests = [
    ( "plus const const",
      plus (aval 2) (aval 3),
      [
        Mini_RISC.LoadI (2, Mini_RISC.RVar 1);
        Mini_RISC.Biop (Mini_RISC.AddI, Mini_RISC.RVar 1, 3, Mini_RISC.RVar 1);
      ] );
    ( "plus var const",
      plus (var "input") (aval 4),
      [Mini_RISC.Biop (Mini_RISC.AddI, Mini_RISC.Rin, 4, Mini_RISC.RVar 1)] );
    ( "plus nested const",
      plus (aval 2) (plus (var "input") (aval 1)),
      [
        Mini_RISC.Biop (Mini_RISC.AddI, Mini_RISC.Rin, 1, Mini_RISC.RVar 1);
        Mini_RISC.Biop (Mini_RISC.AddI, Mini_RISC.RVar 1, 2, Mini_RISC.RVar 2);
      ] );
    ( "minus var const",
      minus (var "input") (aval 4),
      [Mini_RISC.Biop (Mini_RISC.SubI, Mini_RISC.Rin, 4, Mini_RISC.RVar 1)] );
    ( "minus const var",
      minus (aval 4) (var "input"),
      [
        Mini_RISC.LoadI (4, Mini_RISC.RVar 1);
        Mini_RISC.Brop (Mini_RISC.Sub, Mini_RISC.RVar 1, Mini_RISC.Rin, Mini_RISC.RVar 2);
      ] );
    ( "minus expr var",
      minus (plus (var "input") (aval 2)) (var "input"),
      [
        Mini_RISC.Biop (Mini_RISC.AddI, Mini_RISC.Rin, 2, Mini_RISC.RVar 1);
        Mini_RISC.Brop (Mini_RISC.Sub, Mini_RISC.RVar 1, Mini_RISC.Rin, Mini_RISC.RVar 2);
      ] );
    ( "times const const",
      times (aval 2) (aval 3),
      [
        Mini_RISC.LoadI (2, Mini_RISC.RVar 1);
        Mini_RISC.Biop (Mini_RISC.MultI, Mini_RISC.RVar 1, 3, Mini_RISC.RVar 1);
      ] );
    ( "times zero",
      times (aval 0) (var "input"),
      [Mini_RISC.LoadI (0, Mini_RISC.RVar 1)] );
    ( "times one",
      times (var "input") (aval 1),
      [Mini_RISC.Urop (Mini_RISC.Copy, Mini_RISC.Rin, Mini_RISC.RVar 1)] );
    ( "times var expr",
      times (var "input") (plus (aval 1) (aval 2)),
      [
        Mini_RISC.LoadI (1, Mini_RISC.RVar 1);
        Mini_RISC.Biop (Mini_RISC.AddI, Mini_RISC.RVar 1, 2, Mini_RISC.RVar 1);
        Mini_RISC.Brop (Mini_RISC.Mult, Mini_RISC.Rin, Mini_RISC.RVar 1, Mini_RISC.RVar 2);
      ] );
  ] in
  List.iter (fun (name, expr, expected) -> assert_translation name expr expected) tests

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
  ("31_square_inner_loops.mimp", [0; 1; 4; 9; 25; 100; 1; 25]);
  ("32_cube_inner_loops.mimp", [0; 1; 8; 27; 125; 1000; 1; 125]);
  ("33_product_minus_one_inner_loops.mimp", [0; 0; 2; 6; 20; 90; 0; 0]);
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
  run_translation_tests ();

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
