(* IMPORTANT! ASK IF BOOLEAN SHORT-CIRCUIT EVALUATION CAN BE IMPLEMENTED (unneccessary AndI?) *)

(* TODO: 
  1. Remove unnecessary result registers by reusing the final computation register + temporary registers when possible
  2. (Done!) Comment the code 
  3. (Done!) Check if @ can be avoided -> (possible, but makes the code more complex and less readable)
  4. Think about how input and output variables names are associated with Rin and Rout
  5. (Done!) Check if same variable in different nodes are associated with the same register
  6. (Done!) RISC-cfg -> RISC code
*)

(* Main function, given a program compile it into RISC code*)
let compile_from_cfg (program_cfg : Mini_imp_CFG.cfg) (input_var : string) (output_var : string)
    (output_file: string): unit =
  (* Translate the control flow graph into RISC cfg *)
  let (risc_cfg, final_reg_map) = Mini_RISC_CFG.translate_cfg program_cfg input_var output_var in
  (* Write the RISC code by iterating over the nodes *)
  let oc = open_out output_file in
  List.iter (fun (node_id, node) ->
    let label = Mini_RISC_CFG.label_of_node_id node_id in
    let instructions = Mini_RISC_CFG.append_jump risc_cfg node_id node in
    Printf.fprintf oc "%s:\n" label;
    List.iter
      (fun instr -> Printf.fprintf oc "  %s\n" (Mini_imp_Printer.string_of_risc_instruction instr))
      instructions
  ) (Mini_RISC_CFG.NMap.bindings risc_cfg.nodes);
  close_out oc;
  Printf.printf "Final register map: %s\n" (Mini_imp_Printer.reg_map_to_string final_reg_map)

let compile (p: Mini_imp_Interpreter.program) (_input_var: string) (_output_var: string) (output_file: string): unit =
  let program_cfg = Mini_imp_CFG.make_cfg p in
  compile_from_cfg program_cfg p.input p.output output_file

let compile_program (program : Mini_imp_Interpreter.program) (output_file : string) : unit =
  compile program program.input program.output output_file
