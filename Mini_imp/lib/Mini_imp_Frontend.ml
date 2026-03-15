type options = {
  show_tokens : bool;
  show_ast : bool;
  show_cfg : bool;
  show_risc_cfg : bool;
}

let default_options = {
  show_tokens = false;
  show_ast = false;
  show_cfg = false;
  show_risc_cfg = false;
}

let print_token_stream (lexbuf : Lexing.lexbuf) : unit =
  let rec loop () =
    let tok = Mini_imp_Lexer.read lexbuf in
    print_endline (Mini_imp_Printer.string_of_token tok);
    match tok with
    | Mini_imp_Parser.EOF -> ()
    | _ -> loop ()
  in
  loop ()

let parse_program ?(show_tokens = false) (filename : string) : Mini_imp.program =
  if show_tokens then (
    print_endline "=== Tokens ===";
    let ic = open_in filename in
    print_token_stream (Lexing.from_channel ic);
    close_in ic
  );

  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let program = Mini_imp_Parser.prg Mini_imp_Lexer.read lexbuf in
  close_in ic;
  program

let print_program_analysis (opts : options) (program : Mini_imp.program) : Mini_imp_CFG.cfg option =
  if opts.show_ast then (
    print_endline "=== AST ===";
    print_endline (Mini_imp.program_to_string program)
  );

  if opts.show_cfg || opts.show_risc_cfg then (
    let cfg = Mini_imp_CFG.make_cfg program in

    if opts.show_cfg then (
      Printf.printf "=== CFG ===\n";
      Printf.printf "nodes: %d\n" (Mini_imp_CFG.NMap.cardinal cfg.nodes);
      Printf.printf "edges: %d\n" (Mini_imp_CFG.NMap.cardinal cfg.edges);
      Printf.printf "initial: 0\n";
      Printf.printf "final: %s\n"
        (match cfg.final with
        | [ n ] -> string_of_int n
        | _ -> failwith "Unexpected: final should be a single node");
      Printf.printf "%s\n" (Mini_imp_Printer.cfg_to_string cfg)
    );

    if opts.show_risc_cfg then (
      let risc_cfg = Mini_RISC_CFG.translate_cfg cfg program.input program.output in
      Printf.printf "=== RISC CFG ===\n";
      Printf.printf "nodes: %d\n" (Mini_RISC_CFG.NMap.cardinal risc_cfg.nodes);
      Printf.printf "edges: %d\n" (Mini_RISC_CFG.NMap.cardinal risc_cfg.edges);
      Printf.printf "initial: %d\n" risc_cfg.initial;
      Printf.printf "final: %d\n" risc_cfg.final;
      Printf.printf "%s\n" (Mini_imp_Printer.risc_cfg_to_string risc_cfg)
    );

    Some cfg
  ) else None

let options_of_flags ~show_tokens ~show_ast ~show_cfg ~show_risc_cfg : options = {
  show_tokens;
  show_ast;
  show_cfg;
  show_risc_cfg;
}

let analyze_program (opts : options) (program : Mini_imp.program) : Mini_imp_CFG.cfg option =
  print_program_analysis opts program

let analyze_file ?(show_tokens = false) ?(show_ast = false) ?(show_cfg = false)
    ?(show_risc_cfg = false) (filename : string) : Mini_imp.program =
  let opts = options_of_flags ~show_tokens ~show_ast ~show_cfg ~show_risc_cfg in
  let program = parse_program ~show_tokens:opts.show_tokens filename in
  ignore (analyze_program opts program);
  program

let analyze_and_compile_file ?(show_tokens = false) ?(show_ast = false) ?(show_cfg = false)
    ?(show_risc_cfg = false) (filename : string) (output_file : string) : unit =
  let opts = options_of_flags ~show_tokens ~show_ast ~show_cfg ~show_risc_cfg in
  let program = parse_program ~show_tokens:opts.show_tokens filename in
  let cfg_opt = analyze_program opts program in
  let cfg =
    match cfg_opt with
    | Some c -> c
    | None -> Mini_imp_CFG.make_cfg program
  in
  Mini_imp_Compiler.compile_from_cfg cfg program.input program.output output_file
