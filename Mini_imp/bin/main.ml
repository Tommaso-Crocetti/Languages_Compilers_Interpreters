open Mini_imp_Lib.Mini_imp_FlagHandler
open Mini_imp_Lib.Mini_imp_Interpreter
open Mini_imp_Lib.Mini_imp_Compiler

let () =
  let show_tokens = ref false in
  let show_ast = ref false in
  let show_cfg = ref false in
  let show_risc_cfg = ref false in
  let output_file = ref None in
  let filename = ref None in
  let anon_arg arg = filename := Some arg in
  let usage = "Usage: Mini_imp [--tokens] [--ast] [--cfg] [--risc_cfg] [--out output.risc] <program.mimp>" in
  let specs =
    [ ("--tokens", Arg.Set show_tokens, "Print lexer token stream before parsing")
    ; ("--ast", Arg.Set show_ast, "Print parsed program before execution")
    ; ("--cfg", Arg.Set show_cfg, "Print CFG stats after parsing")
    ; ("--risc_cfg", Arg.Set show_risc_cfg, "Print RISC CFG stats after parsing")
    ; ("--out", Arg.String (fun s -> output_file := Some s), "Compile to output file instead of interpreting")
    ]
  in
  Arg.parse specs anon_arg usage;
  match !filename with
  | Some path ->
      (match !output_file with
      | Some out ->
          analyze_and_compile_file
            ~show_tokens:!show_tokens
            ~show_ast:!show_ast
            ~show_cfg:!show_cfg
            ~show_risc_cfg:!show_risc_cfg
            path out;
          Printf.printf "Compiled %s -> %s\n" path out
      | None ->
          let program =
            analyze_file
              ~show_tokens:!show_tokens
              ~show_ast:!show_ast
              ~show_cfg:!show_cfg
              ~show_risc_cfg:!show_risc_cfg
              path
          in
          run_program program)
  | None ->
      prerr_endline usage;
      exit 1
