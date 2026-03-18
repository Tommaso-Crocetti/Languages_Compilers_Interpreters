(* IMPORTANT! ASK IF BOOLEAN SHORT-CIRCUIT EVALUATION CAN BE IMPLEMENTED (unneccessary AndI?) *)

(* TODO: 
  1. Remove unnecessary result registers by reusing the final computation register + temporary registers when possible
  2. (Done!) Comment the code 
  3. (Done!) Check if @ can be avoided -> (possible, but makes the code more complex and less readable)
  4. Think about how input and output variables names are associated with Rin and Rout
  5. (Done!) Check if same variable in different nodes are associated with the same register
  6. (Done!) RISC-cfg -> RISC code
*)

open Mini_imp_AST
open Mini_imp_CFG
open Mini_RISC_CFG
open Mini_imp_Dataflow
open Mini_imp_Printer

(* Main function, given a program compile it into RISC code*)
let compile_from_cfg (program_cfg : cfg) (input_var : string) (output_var : string)
    (output_file: string): unit =
  let _ = defined_analysis program_cfg in
  (* Translate the control flow graph into RISC cfg *)
  let (risc_cfg, final_reg_map) = translate_cfg program_cfg input_var output_var in
  (* Write the RISC code by iterating over the nodes *)
  let oc = open_out output_file in
  List.iter (fun (node_id, node) ->
    let label = label_of_node_id node_id in
    let instructions = append_jump risc_cfg node_id node in
    Printf.fprintf oc "%s:\n" label;
    List.iter
      (fun instr -> Printf.fprintf oc "  %s\n" (string_of_risc_instruction instr))
      instructions
  ) (NMap.bindings risc_cfg.nodes);
  close_out oc;
  Printf.printf "Final register map: %s\n" (reg_map_to_string final_reg_map)

let compile (p: program) (_input_var: string) (_output_var: string) (output_file: string): unit =
  let program_cfg = make_cfg p in
  compile_from_cfg program_cfg p.input_var p.output_var output_file

let compile_program (program : program) (output_file : string) : unit =
  compile program program.input_var program.output_var output_file
