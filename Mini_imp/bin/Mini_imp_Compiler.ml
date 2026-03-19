open Mini_imp_Lib.Mini_imp_Lexer
open Mini_imp_Lib.Mini_imp_Parser
open Mini_imp_Lib.Mini_CFG
open Mini_imp_Lib.Mini_imp_CFG
open Mini_imp_Lib.Mini_RISC
open Mini_imp_Lib.Mini_RISC_CFG
open Mini_imp_Lib.Mini_imp_DefVars
open Mini_imp_Lib.Mini_imp_Printer

let usage =
	"Usage: Mini_imp_compiler [--check-undefined] <input.mimp> <output.risc>"

let parse_program (input_file : string) =
	let ic = open_in input_file in
  let lexbuf = Lexing.from_channel ic in
  let program = prg read lexbuf in
  close_in ic;
  program

let compile_file
	~(check_undefined : bool)
	~(input_file : string)
	~(output_file : string)
	: unit =
	let program = parse_program input_file in
	let cfg = make_cfg program in
	if check_undefined then (
		let _ = defined_analysis cfg in
		Printf.printf "Static analysis completed. All variables are properly defined before use.\n";
	);
	let risc_cfg = translate_cfg cfg in
	let risc_code = risc_cfg_to_code string_of_risc_instruction risc_cfg in
	let oc = open_out output_file in
	output_string oc risc_code;
	close_out oc

let () =
	let check_undefined = ref false in
	let positional = ref [] in

	let specs =
		[ ("--check-undefined", Arg.Set check_undefined, "Enable undefined variable check") ]
	in

	Arg.parse specs (fun arg -> positional := arg :: !positional) usage;

	match List.rev !positional with
	| [input_file; output_file] ->
    (try
			compile_file
        ~check_undefined:!check_undefined
        ~input_file
        ~output_file;
      Printf.printf "Compiled %s -> %s\n" input_file output_file
			with
			  | Sys_error msg
			  | Failure msg ->
					 prerr_endline msg;
					 exit 1
        | Mini_imp_Lib.Mini_imp_Lexer.Error msg ->
            prerr_endline ("Lexing error while reading " ^ input_file ^ ": " ^ msg);
			  | Mini_imp_Lib.Mini_imp_Parser.Error ->
					 prerr_endline "Parse error while reading MiniImp source file.";
					 exit 1
        | Mini_imp_Lib.Mini_imp_CFG.Error msg ->
            prerr_endline ("CFG error: " ^ msg);
        | Mini_imp_Lib.Mini_RISC.Error msg ->
            prerr_endline ("RISC error: " ^ msg);
			  | Mini_imp_Lib.Mini_RISC_CFG.Error msg ->
					 prerr_endline ("RISC CFG error: " ^ msg);
        | Mini_imp_Lib.Mini_imp_DefVars.Error msg ->
            prerr_endline ("Defined variable analysis error: " ^ msg);
					 exit 1)
	| _ ->
			prerr_endline usage;
			exit 2