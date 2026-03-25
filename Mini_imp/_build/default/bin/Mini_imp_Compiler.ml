open Mini_imp_Lib.Mini_imp_Lexer
open Mini_imp_Lib.Mini_imp_Parser
open Mini_imp_Lib.Mini_CFG
open Mini_imp_Lib.Mini_imp_CFG
open Mini_imp_Lib.Mini_RISC
open Mini_imp_Lib.Mini_RISC_CFG
open Mini_imp_Lib.Mini_imp_DefVars
open Mini_imp_Lib.Mini_RISC_LiveRegs
open Mini_imp_Lib.Mini_RISC_RegAlloc
open Mini_imp_Lib.Mini_imp_Printer

let usage =
  "Usage: Mini_imp_compiler [--show-cfg] [--show-risc-cfg] [--def-vars] \
   [--use-live] [--show-colors] [--show-spilled] --max-regs <n>=4 <input.mimp> \
   <output.risc>\n"

let parse_program (input_file : string) =
  let ic = open_in input_file in
  let lexbuf = Lexing.from_channel ic in
  let program = prg read lexbuf in
  close_in ic;
  program

let compile_file ~(show_cfg : bool) ~(show_risc_cfg : bool) ~(def_vars : bool)
    ~(use_live : bool) ~(input_file : string) ~(output_file : string)
    ~(max_regs : int) ~(show_colors : bool) ~(show_spilled : bool) : unit =
  let program = parse_program input_file in
  let cfg = build_cfg program in
  if show_cfg then
    Printf.printf "=== Generated CFG === \n%s\n" (cfg_to_string cfg);
  if def_vars then (
    Printf.printf "=== Defined Variable Analysis ===\n";
    flush stdout;
    let _ = defined_analysis cfg in
    Printf.printf
      "Static analysis completed. All variables are properly defined before use.\n";
    flush stdout
  );
  let risc_cfg, all_vars_reg_map, guard_reg = build_risc_cfg cfg in
  let risc_cfg_with_jumps = risc_cfg_with_jumps guard_reg risc_cfg in
  if show_risc_cfg then
    Printf.printf "=== Generated RISC CFG === \n%s\n"
      (risc_cfg_to_string risc_cfg_with_jumps);
  let df_risc_cfg = build_dataflow_risc_cfg risc_cfg_with_jumps guard_reg in
  let df_risc_cfg =
    if use_live then liveness_global_update df_risc_cfg else df_risc_cfg
  in
  let colored_and_spilled_risc_cfg, final_color_map, spilled_regs =
    global_allocation risc_cfg_with_jumps df_risc_cfg max_regs all_vars_reg_map
      use_live
  in
  if show_colors then
    print_endline (color_map_to_string final_color_map);
  if show_spilled then
    print_endline (spilled_registers_to_string spilled_regs);

  let final_risc_code =
    risc_cfg_to_code colored_and_spilled_risc_cfg string_of_risc_instruction
  in
  let oc = open_out output_file in
  output_string oc final_risc_code;
  close_out oc

let () =
  let show_cfg = ref false in
  let show_risc_cfg = ref false in
  let def_vars = ref false in
  let use_live = ref false in
  let max_regs = ref 4 in
  let show_colors = ref false in
  let show_spilled = ref false in
  let positional = ref [] in

  let specs =
    [
      ("--show-cfg", Arg.Set show_cfg, "Print the generated MiniImp CFG");
      ("--show-risc-cfg", Arg.Set show_risc_cfg, "Print the generated RISC CFG");
      ("--def-vars", Arg.Set def_vars, "Enable undefined variable check");
      ( "--use-live",
        Arg.Set use_live,
        "Use liveness information during register allocation" );
      ("--show-colors", Arg.Set show_colors, "Print the final color map");
      ("--show-spilled", Arg.Set show_spilled, "Print the spilled registers");
      ( "--max-regs",
        Arg.Int
          (fun n ->
            if n < 4 then raise (Arg.Bad "--max-regs must be at least 4")
            else max_regs := n),
        "Set the register budget (>=4)" );
    ]
  in

  Arg.parse specs (fun arg -> positional := arg :: !positional) usage;

  match List.rev !positional with
  | [ input_file; output_file ] -> (
      try
        compile_file ~show_cfg:!show_cfg ~show_risc_cfg:!show_risc_cfg
          ~def_vars:!def_vars ~use_live:!use_live ~input_file ~output_file
          ~max_regs:!max_regs ~show_colors:!show_colors
          ~show_spilled:!show_spilled;
        Printf.printf "Compiled %s -> %s\n" input_file output_file
      with
      | Sys_error msg | Failure msg ->
          prerr_endline msg;
          exit 1
      | Mini_imp_Lib.Mini_imp_Lexer.Error msg ->
          prerr_endline ("Lexing error while reading " ^ input_file ^ ": " ^ msg)
      | Mini_imp_Lib.Mini_imp_Parser.Error ->
          prerr_endline "Parse error while reading MiniImp source file.";
          exit 1
      | Mini_imp_Lib.Mini_imp_CFG.Error msg ->
          prerr_endline ("CFG error: " ^ msg)
      | Mini_imp_Lib.Mini_RISC.Error msg -> prerr_endline ("RISC error: " ^ msg)
      | Mini_imp_Lib.Mini_RISC_CFG.Error msg ->
          prerr_endline ("RISC CFG error: " ^ msg)
      | Mini_imp_Lib.Mini_imp_DefVars.Error msg ->
          prerr_endline ("Defined variable analysis error: " ^ msg);
          exit 1)
  | _ ->
      prerr_endline usage;
      exit 2
