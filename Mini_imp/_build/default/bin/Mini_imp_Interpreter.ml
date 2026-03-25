open Mini_imp_Lib.Mini_imp_Lexer
open Mini_imp_Lib.Mini_imp_Parser
open Mini_imp_Lib.Mini_imp_AST
open Mini_imp_Lib.Mini_imp_Printer

let usage =
  "Usage: Mini_imp_interpreter [--tokens] [--ast] <input.mimp>"

let print_token_stream (input_file : string) =
  let ic = open_in input_file in
  let lexbuf = Lexing.from_channel ic in
  let rec loop () =
    let tok = read lexbuf in
    print_endline (string_of_token tok);
    match tok with
    | EOF -> ()
    | _ -> loop ()
  in
  loop ();
  close_in ic

let parse_program ~(show_tokens : bool) ~(show_ast : bool) (input_file : string) =
  if show_tokens then (
    print_endline "=== Tokens ===";
    print_token_stream input_file
  );

  let ic = open_in input_file in
  let lexbuf = Lexing.from_channel ic in
  let program = prg read lexbuf in
  close_in ic;

  if show_ast then (
    print_endline "=== AST ===";
    print_endline (program_to_string program)
  );

  program

let run_program (program : program) =
  print_endline "=== Input ===";
  let input = read_int () in
  let result = execute program input in
  print_endline "=== Output ===";
  print_int result;
  print_newline ()

let () =
  let show_tokens = ref false in
  let show_ast = ref false in
  let positional = ref [] in
  let specs =
    [ ("--tokens", Arg.Set show_tokens, "Show tokens before parsing")
    ; ("--ast", Arg.Set show_ast, "Show AST before execution")
    ]
  in
  Arg.parse specs (fun arg -> positional := arg :: !positional) usage;

  match List.rev !positional with
  | [input_file] ->
      (try
      	let program =
          parse_program
            ~show_tokens:!show_tokens
            ~show_ast:!show_ast
            input_file
         in
         run_program program
       with
				| Sys_error msg
				| Failure msg ->
           prerr_endline msg;
           exit 1
				| Mini_imp_Lib.Mini_imp_Lexer.Error msg -> 
            prerr_endline ("Lexing error while reading " ^ input_file ^ ": " ^ msg);
      	| Mini_imp_Lib.Mini_imp_Parser.Error ->
           prerr_endline ("Syntax error while reading " ^ input_file);
        | Mini_imp_Lib.Mini_imp_AST.Error msg -> 
            prerr_endline ("Runtime error: " ^ msg);
        exit 1)
	| _ ->
      prerr_endline usage;
      exit 2