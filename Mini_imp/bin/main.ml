open Mini_imp_Lib

let () =
  let show_tokens = ref false in
  let show_ast = ref false in
  let show_cfg = ref false in
  let filename = ref None in
  let anon_arg arg = filename := Some arg in
  let usage = "Usage: Mini_imp [--tokens] [--ast] [--cfg] <program.mimp>" in
  let specs =
    [ ("--tokens", Arg.Set show_tokens, "Print lexer token stream before parsing")
    ; ("--ast", Arg.Set show_ast, "Print parsed program before execution")
    ; ("--cfg", Arg.Set show_cfg, "Print CFG stats after parsing")
    ]
  in
  Arg.parse specs anon_arg usage;
  match !filename with
  | Some path ->
      Mini_imp_Interpreter.run_program
        ~show_tokens:!show_tokens
        ~show_ast:!show_ast
        ~show_cfg:!show_cfg
        path
  | None ->
      prerr_endline usage;
      exit 1
