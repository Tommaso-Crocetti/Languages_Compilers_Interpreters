let execute (t: Mini_tyfun.term) (n: int): int =
  Mini_fun.extract_int (Mini_fun.compute (Mini_fun.app (Mini_tyfun.drop_types t) (Mini_fun.int_ n)))

let run_program (filename: string) = 

  let in_file = open_in filename in
  let lexbuf = Lexing.from_channel in_file in
  let program = Mini_tyfun_Parser.prg Mini_tyfun_Lexer.read lexbuf in
  match Mini_tyfun.type_check program with
    | Some _ -> (
      let input = read_int () in
    print_int (execute program input))
    | None -> failwith "Program is not well-typed"
