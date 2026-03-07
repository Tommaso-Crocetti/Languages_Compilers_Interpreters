open Mini_imp_Lib

let () =
  let in_file = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel in_file in
  let program = Mini_imp_Parser.prg Mini_imp_Lexer.read lexbuf in
  let input = read_int () in
  print_int (Mini_imp.execute program input)