open Mini_imp_Lib
open Mini_imp_AST
open Mini_CFG
open Mini_imp_CFG

let aval n = Mini_imp_AST.Aval n
let var x = Mini_imp_AST.Var x
let plus a1 a2 = Mini_imp_AST.Plus (a1, a2)
let minus a1 a2 = Mini_imp_AST.Minus (a1, a2)
let times a1 a2 = Mini_imp_AST.Times (a1, a2)
let of_bool b = Mini_imp_AST.Of_Bool b

let bval v = Mini_imp_AST.Bval v
let and_ b1 b2 = Mini_imp_AST.And (b1, b2)
let or_ b1 b2 = Mini_imp_AST.Or (b1, b2)
let not_ b = Mini_imp_AST.Not b
let minor a1 a2 = Mini_imp_AST.Minor (a1, a2)

let skip = Mini_imp_AST.Skip
let assign x a = Mini_imp_AST.Assign (x, a)
let seq c1 c2 = Mini_imp_AST.Seq (c1, c2)
let if_ b c1 c2 = Mini_imp_AST.If (b, c1, c2)
let while_ b c = Mini_imp_AST.While (b, c)

let programs_dir = "tests"

let test_inputs = [0; 1; 2; 3; 5; 10; -1; -5]

let string_of_reg = function
  | Mini_RISC.Rin -> "Rin"
  | Mini_RISC.Rout -> "Rout"
  | Mini_RISC.Ra -> "Ra"
  | Mini_RISC.Rb -> "Rb"
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
    | Mini_RISC.Ra -> Mini_RISC.Ra
    | Mini_RISC.Rb -> Mini_RISC.Rb
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
  let (_, actual) = Mini_RISC.translate_aexpr expr None [Mini_RISC.Ra; Mini_RISC.Rb] reg_map in
  let actual = normalize_instructions actual in
  if actual <> expected then
    failwith
      (Printf.sprintf
         "translation test '%s' failed\nexpected: [%s]\nactual:   [%s]"
         name
         (string_of_instructions expected)
         (string_of_instructions actual))

let find_unique_loadi_reg (cfg : Mini_RISC_CFG.risc_cfg) value =
  let matches =
    cfg.nodes
    |> Mini_CFG.NMap.bindings
      |> List.filter_map (fun (_node_id, node_instructions) ->
        match node_instructions with
         | [Mini_RISC.LoadI (n, reg)] when n = value -> Some reg
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

let assert_same_reg name left right =
  if left <> right then
    failwith
      (Printf.sprintf
         "scope test '%s' failed\nexpected same register\nactual: %s <> %s"
         name
         (string_of_reg left)
         (string_of_reg right))

let find_unique_cfg_node (cfg : Mini_imp_CFG.cfg) predicate description =
  let matches =
    cfg.nodes
    |> Mini_CFG.NMap.bindings
    |> List.filter_map (fun (node_id, (statements, _def_vars)) ->
         if predicate statements then Some node_id else None)
  in
  match matches with
  | [node_id] -> node_id
  | [] -> failwith (Printf.sprintf "no CFG node found for %s" description)
  | _ -> failwith (Printf.sprintf "multiple CFG nodes found for %s" description)

let assert_edge_equals (cfg : Mini_imp_CFG.cfg) src expected description =
  match Mini_CFG.NMap.find_opt src cfg.edges with
  | Some actual when actual = expected -> ()
  | Some actual ->
      failwith
        (Printf.sprintf
           "CFG edge mismatch for %s\nexpected: %s\nactual:   %s"
           description
           (Mini_imp_Printer.string_of_cfg_out_node expected)
           (Mini_imp_Printer.string_of_cfg_out_node actual))
  | None -> failwith (Printf.sprintf "missing CFG edge for %s" description)

let edge_of (cfg : Mini_imp_CFG.cfg) src description =
  match Mini_CFG.NMap.find_opt src cfg.edges with
  | Some edge -> edge
  | None -> failwith (Printf.sprintf "missing CFG edge for %s" description)

let run_nested_if_cfg_test () =
  (* Program: if x < 0 then (if true then z:=1 else z:=0) else (if true then x:=1 else x:=2); y := z *)
  let open Mini_imp_AST in
  let prog =
    make_program "x" "y"
      (seq
         (if_
            (minor (var "x") (aval 0))
            (if_ (Bval true) (assign "z" (aval 1)) (assign "z" (aval 0)))
            (if_ (Bval true) (assign "x" (aval 1)) (assign "x" (aval 2))))
         (assign "y" (var "z")))
  in
  let cfg = Mini_imp_CFG.make_cfg prog in
  let outer_guard_node =
    find_unique_cfg_node
      cfg
      (function
          | [Mini_imp_CFG.Guard (Mini_imp_AST.Minor
            (Mini_imp_AST.Var "x", Mini_imp_AST.Aval 0))] -> true
        | _ -> false)
      "outer guard"
  in
  let inner_then_guard_node, outer_else_guard_node =
    match edge_of cfg outer_guard_node "outer guard" with
    | Mini_CFG.Pair (left, right) -> (left, right)
    | Mini_CFG.Single _ -> failwith "outer guard should have a Pair edge"
  in
  let assign_z_1 =
    find_unique_cfg_node
      cfg
      (function [Mini_imp_CFG.Assign ("z", Mini_imp_AST.Aval 1)] -> true | _ -> false)
      "z := 1"
  in
  let assign_z_0 =
    find_unique_cfg_node
      cfg
      (function [Mini_imp_CFG.Assign ("z", Mini_imp_AST.Aval 0)] -> true | _ -> false)
      "z := 0"
  in
  let assign_x_1 =
    find_unique_cfg_node
      cfg
      (function [Mini_imp_CFG.Assign ("x", Mini_imp_AST.Aval 1)] -> true | _ -> false)
      "x := 1"
  in
  let assign_x_2 =
    find_unique_cfg_node
      cfg
      (function [Mini_imp_CFG.Assign ("x", Mini_imp_AST.Aval 2)] -> true | _ -> false)
      "x := 2"
  in
  let final_node =
    find_unique_cfg_node
      cfg
      (function [Mini_imp_CFG.Assign ("y", Mini_imp_AST.Var "z")] -> true | _ -> false)
      "y := z"
  in
  assert_edge_equals cfg inner_then_guard_node (Mini_CFG.Pair (assign_z_1, assign_z_0)) "inner then guard";
  assert_edge_equals cfg outer_else_guard_node (Mini_CFG.Pair (assign_x_1, assign_x_2)) "outer else guard";
  assert_edge_equals cfg assign_z_1 (Mini_CFG.Single final_node) "z:=1 exit";
  assert_edge_equals cfg assign_z_0 (Mini_CFG.Single final_node) "z:=0 exit";
  assert_edge_equals cfg assign_x_1 (Mini_CFG.Single final_node) "x:=1 exit";
  assert_edge_equals cfg assign_x_2 (Mini_CFG.Single final_node) "x:=2 exit"

let run_cfg_scope_tests () =
  let open Mini_imp_AST in
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
  let if_risc = Mini_RISC_CFG.translate_cfg if_cfg in
  let then_reg = find_unique_loadi_reg if_risc 41 in
  let join_reg = find_unique_loadi_reg if_risc 99 in
  assert_same_reg "if variable assignment is preserved at join" then_reg join_reg

let run_translation_tests () =
  let open Mini_imp_AST in
  let tests = [
    ( "plus const const",
      plus (aval 2) (aval 3),
      [
        Mini_RISC.LoadI (5, Mini_RISC.Ra);
      ] );
    ( "plus var const",
      plus (var "input") (aval 4),
      [Mini_RISC.Biop (Mini_RISC.AddI, Mini_RISC.Rin, 4, Mini_RISC.Ra)] );
    ( "plus nested const",
      plus (aval 2) (plus (var "input") (aval 1)),
      [
        Mini_RISC.Biop (Mini_RISC.AddI, Mini_RISC.Rin, 1, Mini_RISC.Ra);
        Mini_RISC.Biop (Mini_RISC.AddI, Mini_RISC.Ra, 2, Mini_RISC.Ra);
      ] );
    ( "minus var const",
      minus (var "input") (aval 4),
      [Mini_RISC.Biop (Mini_RISC.SubI, Mini_RISC.Rin, 4, Mini_RISC.Ra)] );
    ( "minus const var",
      minus (aval 4) (var "input"),
      [
        Mini_RISC.LoadI (4, Mini_RISC.Ra);
        Mini_RISC.Brop (Mini_RISC.Sub, Mini_RISC.Ra, Mini_RISC.Rin, Mini_RISC.Ra);
      ] );
    ( "minus expr var",
      minus (plus (var "input") (aval 2)) (var "input"),
      [
        Mini_RISC.Biop (Mini_RISC.AddI, Mini_RISC.Rin, 2, Mini_RISC.Ra);
        Mini_RISC.Brop (Mini_RISC.Sub, Mini_RISC.Ra, Mini_RISC.Rin, Mini_RISC.Ra);
      ] );
    ( "times const const",
      times (aval 2) (aval 3),
      [
        Mini_RISC.LoadI (6, Mini_RISC.Ra);
      ] );
    ( "times zero",
      times (aval 0) (var "input"),
      [Mini_RISC.LoadI (0, Mini_RISC.Ra)] );
    ( "times one",
      times (var "input") (aval 1),
      [Mini_RISC.Urop (Mini_RISC.Copy, Mini_RISC.Rin, Mini_RISC.Ra)] );
    ( "times var expr",
      times (var "input") (plus (aval 1) (aval 2)),
      [
        Mini_RISC.LoadI (3, Mini_RISC.Ra);
        Mini_RISC.Brop (Mini_RISC.Mult, Mini_RISC.Rin, Mini_RISC.Ra, Mini_RISC.Ra);
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
                  let result = Mini_imp_AST.execute prog n in
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
  run_nested_if_cfg_test ();
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
