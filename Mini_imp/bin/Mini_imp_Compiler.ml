open Mini_imp_Lib.Mini_imp_Lexer
open Mini_imp_Lib.Mini_imp_Parser
open Mini_imp_Lib.Mini_CFG
open Mini_imp_Lib.Mini_imp_CFG
open Mini_imp_Lib.Mini_RISC
open Mini_imp_Lib.Mini_RISC_CFG
open Mini_imp_Lib.Mini_imp_DefVars
open Mini_imp_Lib.Mini_RISC_LiveRegs
open Mini_imp_Lib.Mini_imp_Printer

let usage =
	"Usage: Mini_imp_compiler [--show-cfg] [--check-undefined] [--print-liveness] <input.mimp> <output.risc>"

let parse_program (input_file : string) =
	let ic = open_in input_file in
  let lexbuf = Lexing.from_channel ic in
  let program = prg read lexbuf in
  close_in ic;
  program

let compile_file
  ~(show_cfg : bool)
	~(check_undefined : bool)
  ~(print_liveness : bool)
	~(input_file : string)
	~(output_file : string)
	: unit =
	let program = parse_program input_file in
	let cfg = build_cfg program in
  if show_cfg then (
    Printf.printf "Generated CFG:\n%s\n" (cfg_to_string cfg);
  );
	if check_undefined then (
		let _ = defined_analysis cfg in
		Printf.printf "Static analysis completed. All variables are properly defined before use.\n";
	);
  let risc_cfg = build_risc_cfg cfg in
  if print_liveness then (
    let df_risc_cfg = build_dataflow_risc_cfg risc_cfg in
    let liveness_info = liveness_global_update df_risc_cfg in
    Printf.printf "Performing liveness analysis...\n";
    print_in_out_regs liveness_info;
  );
	let risc_code = risc_cfg_to_code string_of_risc_instruction risc_cfg in
	let oc = open_out output_file in
	output_string oc risc_code;
	close_out oc

let () =
  let show_cfg = ref false in
	let check_undefined = ref false in
	let print_liveness = ref false in
	let positional = ref [] in

	let specs =
		[
      ("--show-cfg", Arg.Set show_cfg, "Print the generated CFG");
      ("--check-undefined", Arg.Set check_undefined, "Enable undefined variable check");
      ("--print-liveness", Arg.Set print_liveness, "Print liveness information");
    ]
	in

	Arg.parse specs (fun arg -> positional := arg :: !positional) usage;

	match List.rev !positional with
	| [input_file; output_file] ->
    (try
			compile_file
        ~show_cfg:!show_cfg
        ~check_undefined:!check_undefined
        ~print_liveness:!print_liveness
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
