let execute (t: Mini_tyfun.term) (n: int): int =
  Mini_fun.extract_int (Mini_fun.compute (Mini_fun.app (Mini_tyfun.drop_types t) (Mini_fun.int_ n)))

let string_of_token (tok : Mini_tyfun_Parser.token) : string =
  match tok with
  | Mini_tyfun_Parser.INT n -> "INT(" ^ string_of_int n ^ ")"
  | Mini_tyfun_Parser.BOOL b -> "BOOL(" ^ string_of_bool b ^ ")"
  | Mini_tyfun_Parser.VAR x -> "VAR(" ^ x ^ ")"
  | Mini_tyfun_Parser.PLUS -> "PLUS"
  | Mini_tyfun_Parser.MINUS -> "MINUS"
  | Mini_tyfun_Parser.TIMES -> "TIMES"
  | Mini_tyfun_Parser.AND -> "AND"
  | Mini_tyfun_Parser.OR -> "OR"
  | Mini_tyfun_Parser.NOT -> "NOT"
  | Mini_tyfun_Parser.MINOR -> "MINOR"
  | Mini_tyfun_Parser.LET -> "LET"
  | Mini_tyfun_Parser.EQUAL -> "EQUAL"
  | Mini_tyfun_Parser.IN -> "IN"
  | Mini_tyfun_Parser.LETFUN -> "LETFUN"
  | Mini_tyfun_Parser.COLON -> "COLON"
  | Mini_tyfun_Parser.FUN -> "FUN"
  | Mini_tyfun_Parser.FUN_ARROW -> "FUN_ARROW"
  | Mini_tyfun_Parser.IF -> "IF"
  | Mini_tyfun_Parser.THEN -> "THEN"
  | Mini_tyfun_Parser.ELSE -> "ELSE"
  | Mini_tyfun_Parser.LPAREN -> "LPAREN"
  | Mini_tyfun_Parser.RPAREN -> "RPAREN"
  | Mini_tyfun_Parser.INT_TYPE -> "INT_TYPE"
  | Mini_tyfun_Parser.BOOL_TYPE -> "BOOL_TYPE"
  | Mini_tyfun_Parser.ARROW_TYPE -> "ARROW_TYPE"
  | Mini_tyfun_Parser.EOF -> "EOF"

let print_token_stream (lexbuf : Lexing.lexbuf) : unit =
  let rec loop () =
    let tok = Mini_tyfun_Lexer.read lexbuf in
    print_endline (string_of_token tok);
    match tok with
    | Mini_tyfun_Parser.EOF -> ()
    | _ -> loop ()
  in
  loop ()

let run_program ?(show_tokens = false) ?(show_ast = false) (filename: string) =
  if show_tokens then (
    print_endline "=== Tokens ===";
    let ic = open_in filename in
    print_token_stream (Lexing.from_channel ic);
    close_in ic);

  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let program = Mini_tyfun_Parser.prg Mini_tyfun_Lexer.read lexbuf in
  close_in ic;

  if show_ast then (
    print_endline "=== AST ===";
    print_endline (Mini_tyfun.ast_to_string program);
  );

  match Mini_tyfun.type_check program with
    | Some _ -> (
      print_endline "=== Input ===";
      flush stdout;
      let input = read_int () in
      print_endline "=== Output ===";
      print_int (execute program input);
      print_newline ())
    | None -> failwith "Program is not well-typed"