open Mini_imp_Lib

let () =
  let show_tokens = ref false in
  let show_program = ref false in
  let filename = ref None in
  let anon_arg arg = filename := Some arg in
  let usage = "Usage: Mini_imp [--tokens] [--program] <program.mimp>" in
  let specs =
    [ ("--tokens", Arg.Set show_tokens, "Print lexer token stream before parsing")
    ; ("--program", Arg.Set show_program, "Print parsed program before execution")
    ]
  in
  Arg.parse specs anon_arg usage;
  match !filename with
  | Some path -> Mini_imp_Interpreter.run_program ~show_tokens:!show_tokens ~show_program:!show_program path
  | None ->
      prerr_endline usage;
      exit 1
