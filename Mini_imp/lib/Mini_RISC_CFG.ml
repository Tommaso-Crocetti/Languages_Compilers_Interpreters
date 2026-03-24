open Mini_CFG
open Mini_imp_CFG
open Mini_RISC

exception Error of string

module SMap = Mini_RISC.SMap

type risc_cfg = instruction list generic_cfg

(* Main function, translates a CFG to a RISC CFG by traversing the original one *)
let build_risc_cfg (cfg : cfg) : risc_cfg * var_to_reg * reg =
  let initial_reg_map = initial_reg_map cfg.input_var cfg.output_var in
  let all_vars_reg_map =
    SSet.fold
      (fun var acc ->
        if SMap.mem var acc then acc else SMap.add var (fresh_reg ()) acc)
      cfg.all_vars initial_reg_map
  in
  let guard_reg = fresh_reg () in
  let other_temp_reg = fresh_reg () in
  let temp_checkpoint = reg_allocator_checkpoint () in
  let temp_regs = [ guard_reg; other_temp_reg ] in
  let risc_nodes =
    IMap.mapi
      (fun node_id stmts ->
        reset_reg_allocator temp_checkpoint;
        let instructions, _ =
          translate_stmts stmts temp_regs guard_reg all_vars_reg_map
        in
        instructions)
      cfg.nodes
  in
  ( {
      nodes = risc_nodes;
      edges = cfg.edges;
      initial = cfg.initial;
      final = cfg.final;
      input_var = cfg.input_var;
      output_var = cfg.output_var;
      all_vars = cfg.all_vars;
    },
    all_vars_reg_map,
    guard_reg )

(* Helper function that generates the label for a node Id *)
let string_of_label (node_id : int) : string =
  if node_id = 0 then "main" else Printf.sprintf "l%d" node_id

(* Helper function that appends a jump instruction representing the outgoing edge of the node *)
let append_jump (guard_reg : reg) (cfg : risc_cfg) (node_id : int)
    (instrs : instruction list) : instruction list =
  match IMap.find_opt node_id cfg.edges with
  (* If there is no outgoing edge, the final node has been reached *)
  | None -> instrs
  (* If there is a single outgoing edge, append a jump *)
  | Some (Single next_node) -> instrs @ [ Jump (string_of_label next_node) ]
  (* If there is a pair of outgoing edges,
  append a conditional jump based on the guard register*)
  | Some (Pair (left_node, right_node)) ->
      instrs
      @ [
          CJump
            (guard_reg, string_of_label left_node, string_of_label right_node);
        ]

let risc_cfg_with_jumps (guard_reg : reg) (risc_cfg : risc_cfg) : risc_cfg =
  let nodes_with_jumps =
    IMap.mapi
      (fun node_id instrs -> append_jump guard_reg risc_cfg node_id instrs)
      risc_cfg.nodes
  in
  { risc_cfg with nodes = nodes_with_jumps }

let risc_cfg_to_code (string_of_instruction : instruction -> string)
    (guard_reg : reg) (cfg : risc_cfg) : string =
  let cfg = risc_cfg_with_jumps guard_reg cfg in
  List.fold_left
    (fun acc (node_id, instrs) ->
      let label = string_of_label node_id in
      acc ^ label ^ ":\n"
      ^ String.concat "\n"
          (List.map
             (fun instr -> "\t" ^ string_of_instruction instr)
             (IMap.find node_id cfg.nodes))
      ^ "\n" ^ "\n")
    "" (IMap.bindings cfg.nodes)
