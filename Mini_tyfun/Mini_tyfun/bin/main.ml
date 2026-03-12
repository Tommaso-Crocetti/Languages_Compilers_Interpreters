open Mini_tyfun_Lib

let () =
  let show_tokens = ref false in
  let show_ast = ref false in
  let filename = ref None in
  let anon_arg arg = filename := Some arg in
  let usage = "Usage: mini_tyfun [--tokens] [--ast] <program.mtfun>" in
  let specs =
    [ ("--tokens", Arg.Set show_tokens, "Print lexer token stream before parsing")
    ; ("--ast", Arg.Set show_ast, "Print parsed AST before execution")
    ]
  in
  Arg.parse specs anon_arg usage;
  match !filename with
  | Some path -> Mini_tyfun_Interpreter.run_program ~show_tokens:!show_tokens ~show_ast:!show_ast path
  | None ->
      prerr_endline usage;
      exit 1
