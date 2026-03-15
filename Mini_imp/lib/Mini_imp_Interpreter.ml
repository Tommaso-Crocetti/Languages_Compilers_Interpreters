let string_of_token (tok : Mini_imp_Parser.token) : string =
  match tok with
  | Mini_imp_Parser.DEF -> "DEF"
  | Mini_imp_Parser.MAIN -> "MAIN"
  | Mini_imp_Parser.WITH -> "WITH"
  | Mini_imp_Parser.INPUT -> "INPUT"
  | Mini_imp_Parser.OUTPUT -> "OUTPUT"
  | Mini_imp_Parser.AS -> "AS"
  | Mini_imp_Parser.INT n -> "INT(" ^ string_of_int n ^ ")"
  | Mini_imp_Parser.BOOL b -> "BOOL(" ^ string_of_bool b ^ ")"
  | Mini_imp_Parser.VAR x -> "VAR(" ^ x ^ ")"
  | Mini_imp_Parser.PLUS -> "PLUS"
  | Mini_imp_Parser.MINUS -> "MINUS"
  | Mini_imp_Parser.TIMES -> "TIMES"
  | Mini_imp_Parser.OF_BOOL -> "OF_BOOL"
  | Mini_imp_Parser.AND -> "AND"
  | Mini_imp_Parser.OR -> "OR"
  | Mini_imp_Parser.NOT -> "NOT"
  | Mini_imp_Parser.MINOR -> "MINOR"
  | Mini_imp_Parser.SKIP -> "SKIP"
  | Mini_imp_Parser.ASSIGN -> "ASSIGN"
  | Mini_imp_Parser.CONCAT -> "CONCAT"
  | Mini_imp_Parser.IF -> "IF"
  | Mini_imp_Parser.THEN -> "THEN"
  | Mini_imp_Parser.ELSE -> "ELSE"
  | Mini_imp_Parser.WHILE -> "WHILE"
  | Mini_imp_Parser.DO -> "DO"
  | Mini_imp_Parser.LPAREN -> "LPAREN"
  | Mini_imp_Parser.RPAREN -> "RPAREN"
  | Mini_imp_Parser.EOF -> "EOF"

let print_token_stream (lexbuf : Lexing.lexbuf) : unit =
  let rec loop () =
    let tok = Mini_imp_Lexer.read lexbuf in
    print_endline (string_of_token tok);
    match tok with
    | Mini_imp_Parser.EOF -> ()
    | _ -> loop ()
  in
  loop ()

let run_program ?(show_tokens = false) ?(show_ast = false) ?(show_cfg = false) (filename: string) =
  if show_tokens then (
    print_endline "=== Tokens ===";
    let ic = open_in filename in
    print_token_stream (Lexing.from_channel ic);
    close_in ic);

  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let program = Mini_imp_Parser.prg Mini_imp_Lexer.read lexbuf in
  close_in ic;
  
  if show_ast then (
    print_endline "=== AST ===";
    print_endline (Mini_imp.program_to_string program);
  );

  (* Build CFG eagerly so the analysis pipeline is exercised on every run. *)
  let cfg = Mini_imp_CFG.make_cfg program in

  if show_cfg then (
    Printf.printf "=== CFG ===\n";
    Printf.printf "nodes: %d\n" (Mini_imp_CFG.NMap.cardinal cfg.nodes);
    Printf.printf "edges: %d\n" (Mini_imp_CFG.NMap.cardinal cfg.edges);
    Printf.printf "initial: 0\n";
    Printf.printf "final: %s\n" (
      match cfg.final with
      | [n]-> string_of_int n
      | _ -> failwith "Unexpected: final should be a single node"
    );
    Printf.printf "%s\n" (Mini_imp_CFG.cfg_to_string cfg);
  );

  print_endline "=== Input ===";
  let input = read_int () in
  print_endline "=== Output ===";
  print_int (Mini_imp.execute program input);
  print_newline ()
