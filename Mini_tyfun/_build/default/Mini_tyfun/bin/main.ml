open Mini_tyfun_Lib

let () = 
  let filename = Sys.argv.(1) in
  let channel = open_in filename in
  let lexbuf = Lexing.from_channel channel in
  let program = Mini_tyfun_Parser.prg Mini_tyfun_Lexer.read lexbuf in
  Mini_tyfun.print_AST program;
  print_newline ();
  Mini_tyfun_Interpreter.run_program filename
