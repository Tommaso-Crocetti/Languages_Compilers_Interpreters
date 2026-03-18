open Mini_imp_Lexer
open Mini_imp_Parser
open Mini_imp_AST
open Mini_imp_CFG
open Mini_RISC_CFG
open Mini_imp_Compiler
open Mini_imp_Dataflow
open Mini_imp_Printer


exception Error of string

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
    let tok = read lexbuf in
    print_endline (string_of_token tok);
    match tok with
    | EOF -> ()
    | _ -> loop ()
  in
  loop ()

let parse_program ?(show_tokens = false) (filename : string) : program =
  if show_tokens then (
    print_endline "=== Tokens ===";
    let ic = open_in filename in
    print_token_stream (Lexing.from_channel ic);
    close_in ic
  );

  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let program = prg read lexbuf in
  close_in ic;
  program

let print_program_analysis (opts : options) (program : program) : cfg option =
  if opts.show_ast then (
    print_endline "=== AST ===";
    print_endline (program_to_string program)
  );

  if opts.show_cfg || opts.show_risc_cfg then (
    let cfg = make_cfg program in

    if opts.show_cfg then (
      Printf.printf "=== CFG ===\n";
      Printf.printf "nodes: %d\n" (NMap.cardinal cfg.nodes);
      Printf.printf "edges: %d\n" (NMap.cardinal cfg.edges);
      Printf.printf "initial: 0\n";
      Printf.printf "final: %s\n"
        (match cfg.final with
        | [ n ] -> string_of_int n
        | _ -> raise (Error "Unexpected: final should be a single node"));
      Printf.printf "%s\n" (cfg_to_string cfg)
    );

    if opts.show_risc_cfg then (
      let (risc_cfg, final_reg_map) = translate_cfg cfg program.input_var program.output_var in
      Printf.printf "=== RISC CFG ===\n";
      Printf.printf "nodes: %d\n" (NMap.cardinal risc_cfg.nodes);
      Printf.printf "edges: %d\n" (NMap.cardinal risc_cfg.edges);
      Printf.printf "initial: %d\n" risc_cfg.initial;
      Printf.printf "final: %s\n" (String.concat ", " (List.map string_of_int risc_cfg.final));
      Printf.printf "%s\n"
        (risc_cfg_and_reg_map_to_string risc_cfg final_reg_map)
    );

    Some cfg
  ) else None

let options_of_flags ~show_tokens ~show_ast ~show_cfg ~show_risc_cfg : options = {
  show_tokens;
  show_ast;
  show_cfg;
  show_risc_cfg;
}

let analyze_program (opts : options) (program : program) : cfg option =
  print_program_analysis opts program

let analyze_file ?(show_tokens = false) ?(show_ast = false) ?(show_cfg = false)
    ?(show_risc_cfg = false) (filename : string) : program =
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
    | None -> make_cfg program
  in
  compile_from_cfg cfg program.input_var program.output_var output_file