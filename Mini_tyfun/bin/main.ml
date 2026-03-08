open Mini_tyfun_Lib

let () =
  let in_file = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel in_file in
  let program = Mini_tyfun_Parser.prg Mini_tyfun_Lexer.read lexbuf in
  let input = read_int () in
  print_int (Mini_tyfun.execute program input)