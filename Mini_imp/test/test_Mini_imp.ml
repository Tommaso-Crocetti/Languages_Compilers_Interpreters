open Mini_imp_Lib

let programs_dir = "tests"

let test_inputs = [0; 1; 2; 3; 5; 10; -1; -5]

let string_of_reg = function
  | Mini_RISC_CFG.Rin -> "Rin"
  | Mini_RISC_CFG.Rout -> "Rout"
  | Mini_RISC_CFG.Ra -> "Ra"
  | Mini_RISC_CFG.Rb -> "Rb"
  | Mini_RISC_CFG.RVar n -> "R" ^ string_of_int n

let string_of_brop = function
  | Mini_RISC_CFG.Add -> "Add"
  | Mini_RISC_CFG.Sub -> "Sub"
  | Mini_RISC_CFG.Mult -> "Mult"
  | Mini_RISC_CFG.And -> "And"
  | Mini_RISC_CFG.Less -> "Less"
  | Mini_RISC_CFG.Or -> "Or"

let string_of_biop = function
  | Mini_RISC_CFG.AddI -> "AddI"
  | Mini_RISC_CFG.SubI -> "SubI"
  | Mini_RISC_CFG.MultI -> "MultI"
  | Mini_RISC_CFG.AndI -> "AndI"
  | Mini_RISC_CFG.OrI -> "OrI"

let string_of_urop = function
  | Mini_RISC_CFG.Not -> "Not"
  | Mini_RISC_CFG.Copy -> "Copy"

let string_of_instruction = function
  | Mini_RISC_CFG.Nop -> "Nop"
  | Mini_RISC_CFG.Brop (op, left, right, dst) ->
      Printf.sprintf "Brop(%s,%s,%s,%s)"
        (string_of_brop op)
        (string_of_reg left)
        (string_of_reg right)
        (string_of_reg dst)
  | Mini_RISC_CFG.Biop (op, src, imm, dst) ->
      Printf.sprintf "Biop(%s,%s,%d,%s)"
        (string_of_biop op)
        (string_of_reg src)
        imm
        (string_of_reg dst)
  | Mini_RISC_CFG.Urop (op, src, dst) ->
      Printf.sprintf "Urop(%s,%s,%s)"
        (string_of_urop op)
        (string_of_reg src)
        (string_of_reg dst)
  | Mini_RISC_CFG.Load (src, dst) ->
      Printf.sprintf "Load(%s,%s)" (string_of_reg src) (string_of_reg dst)
  | Mini_RISC_CFG.LoadI (n, dst) ->
      Printf.sprintf "LoadI(%d,%s)" n (string_of_reg dst)
  | Mini_RISC_CFG.Store (src, dst) ->
      Printf.sprintf "Store(%s,%s)" (string_of_reg src) (string_of_reg dst)
  | Mini_RISC_CFG.Jump label -> Printf.sprintf "Jump(%s)" label
  | Mini_RISC_CFG.CJump (cond, l1, l2) ->
      Printf.sprintf "CJump(%s,%s,%s)" (string_of_reg cond) l1 l2

let string_of_instructions code =
  code
  |> List.map string_of_instruction
  |> String.concat "; "

let normalize_instructions code =
  let next = ref 0 in
  let seen = Hashtbl.create 8 in
  let normalize_reg = function
    | Mini_RISC_CFG.Rin -> Mini_RISC_CFG.Rin
    | Mini_RISC_CFG.Rout -> Mini_RISC_CFG.Rout
    | Mini_RISC_CFG.Ra -> Mini_RISC_CFG.Ra
    | Mini_RISC_CFG.Rb -> Mini_RISC_CFG.Rb
    | Mini_RISC_CFG.RVar n ->
        (match Hashtbl.find_opt seen n with
        | Some mapped -> Mini_RISC_CFG.RVar mapped
        | None ->
            incr next;
            Hashtbl.add seen n !next;
            Mini_RISC_CFG.RVar !next)
  in
  let normalize_instruction = function
    | Mini_RISC_CFG.Nop -> Mini_RISC_CFG.Nop
    | Mini_RISC_CFG.Brop (op, left, right, dst) ->
      Mini_RISC_CFG.Brop (op, normalize_reg left, normalize_reg right, normalize_reg dst)
    | Mini_RISC_CFG.Biop (op, src, imm, dst) ->
        Mini_RISC_CFG.Biop (op, normalize_reg src, imm, normalize_reg dst)
    | Mini_RISC_CFG.Urop (op, src, dst) ->
        Mini_RISC_CFG.Urop (op, normalize_reg src, normalize_reg dst)
    | Mini_RISC_CFG.Load (src, dst) ->
        Mini_RISC_CFG.Load (normalize_reg src, normalize_reg dst)
    | Mini_RISC_CFG.LoadI (n, dst) ->
        Mini_RISC_CFG.LoadI (n, normalize_reg dst)
    | Mini_RISC_CFG.Store (src, dst) ->
        Mini_RISC_CFG.Store (normalize_reg src, normalize_reg dst)
    | Mini_RISC_CFG.Jump label -> Mini_RISC_CFG.Jump label
    | Mini_RISC_CFG.CJump (cond, l1, l2) ->
        Mini_RISC_CFG.CJump (normalize_reg cond, l1, l2)
  in
  List.map normalize_instruction code

let assert_translation name expr expected =
  let reg_map = Mini_RISC_CFG.initial_reg_map "input" "output" in
  let (_, actual) = Mini_RISC_CFG.translate_aexpr expr None reg_map in
  let actual = normalize_instructions actual in
  if actual <> expected then
    failwith
      (Printf.sprintf
         "translation test '%s' failed\nexpected: [%s]\nactual:   [%s]"
         name
         (string_of_instructions expected)
         (string_of_instructions actual))

let find_unique_loadi_reg cfg value =
  let matches =
    cfg.Mini_RISC_CFG.nodes
    |> Mini_imp_CFG.NMap.bindings
    |> List.filter_map (fun (_node_id, node) ->
         match node.Mini_RISC_CFG.instructions with
         | [Mini_RISC_CFG.LoadI (n, reg)] when n = value -> Some reg
         | _ -> None)
  in
  match matches with
  | [reg] -> reg
  | [] -> failwith (Printf.sprintf "no unique LoadI node found for constant %d" value)
  | _ -> failwith (Printf.sprintf "multiple LoadI nodes found for constant %d" value)

let assert_distinct_regs name left right =
  if left = right then
    failwith
      (Printf.sprintf
         "scope test '%s' failed\nexpected distinct registers\nactual: %s = %s"
         name
         (string_of_reg left)
         (string_of_reg right))

let run_cfg_scope_tests () =
  let open Mini_imp in
  let if_prog =
    make_program
      "input"
      "output"
      (seq
         (if_
            (minor (var "input") (aval 0))
            (assign "tmp" (aval 41))
            skip)
         (assign "tmp" (aval 99)))
  in
  let if_cfg = Mini_imp_CFG.make_cfg if_prog in
  let if_risc = Mini_RISC_CFG.translate_cfg if_cfg "input" "output" in
  let then_reg = find_unique_loadi_reg if_risc 41 in
  let join_reg = find_unique_loadi_reg if_risc 99 in
  assert_distinct_regs "if local does not escape join" then_reg join_reg

let run_translation_tests () =
  let open Mini_imp in
  let tests = [
    ( "plus const const",
      plus (aval 2) (aval 3),
      [
        Mini_RISC_CFG.LoadI (2, Mini_RISC_CFG.RVar 1);
        Mini_RISC_CFG.Biop (Mini_RISC_CFG.AddI, Mini_RISC_CFG.RVar 1, 3, Mini_RISC_CFG.RVar 1);
      ] );
    ( "plus var const",
      plus (var "input") (aval 4),
      [Mini_RISC_CFG.Biop (Mini_RISC_CFG.AddI, Mini_RISC_CFG.Rin, 4, Mini_RISC_CFG.RVar 1)] );
    ( "plus nested const",
      plus (aval 2) (plus (var "input") (aval 1)),
      [
        Mini_RISC_CFG.Biop (Mini_RISC_CFG.AddI, Mini_RISC_CFG.Rin, 1, Mini_RISC_CFG.RVar 1);
        Mini_RISC_CFG.Biop (Mini_RISC_CFG.AddI, Mini_RISC_CFG.RVar 1, 2, Mini_RISC_CFG.RVar 2);
      ] );
    ( "minus var const",
      minus (var "input") (aval 4),
      [Mini_RISC_CFG.Biop (Mini_RISC_CFG.SubI, Mini_RISC_CFG.Rin, 4, Mini_RISC_CFG.RVar 1)] );
    ( "minus const var",
      minus (aval 4) (var "input"),
      [
        Mini_RISC_CFG.LoadI (4, Mini_RISC_CFG.RVar 1);
        Mini_RISC_CFG.Brop (Mini_RISC_CFG.Sub, Mini_RISC_CFG.RVar 1, Mini_RISC_CFG.Rin, Mini_RISC_CFG.RVar 1);
      ] );
    ( "minus expr var",
      minus (plus (var "input") (aval 2)) (var "input"),
      [
        Mini_RISC_CFG.Biop (Mini_RISC_CFG.AddI, Mini_RISC_CFG.Rin, 2, Mini_RISC_CFG.RVar 1);
        Mini_RISC_CFG.Brop (Mini_RISC_CFG.Sub, Mini_RISC_CFG.RVar 1, Mini_RISC_CFG.Rin, Mini_RISC_CFG.RVar 2);
      ] );
    ( "times const const",
      times (aval 2) (aval 3),
      [
        Mini_RISC_CFG.LoadI (2, Mini_RISC_CFG.RVar 1);
        Mini_RISC_CFG.Biop (Mini_RISC_CFG.MultI, Mini_RISC_CFG.RVar 1, 3, Mini_RISC_CFG.RVar 1);
      ] );
    ( "times zero",
      times (aval 0) (var "input"),
      [Mini_RISC_CFG.LoadI (0, Mini_RISC_CFG.RVar 1)] );
    ( "times one",
      times (var "input") (aval 1),
      [Mini_RISC_CFG.Urop (Mini_RISC_CFG.Copy, Mini_RISC_CFG.Rin, Mini_RISC_CFG.RVar 1)] );
    ( "times var expr",
      times (var "input") (plus (aval 1) (aval 2)),
      [
        Mini_RISC_CFG.LoadI (1, Mini_RISC_CFG.RVar 1);
        Mini_RISC_CFG.Biop (Mini_RISC_CFG.AddI, Mini_RISC_CFG.RVar 1, 2, Mini_RISC_CFG.RVar 1);
        Mini_RISC_CFG.Brop (Mini_RISC_CFG.Mult, Mini_RISC_CFG.Rin, Mini_RISC_CFG.RVar 1, Mini_RISC_CFG.RVar 2);
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
  run_cfg_scope_tests ();
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
