open Mini_CFG
open Mini_imp_CFG
open Mini_RISC

exception Error of string

module SMap = Mini_RISC.SMap

(** A RISC CFG is a generic CFG where nodes are associated to lists of RISC instructions *)
type risc_cfg = instruction list generic_cfg

(** Turns a CFG to a RISC CFG by translating the list of maximal statements of the original CFG *)
let build_risc_cfg (cfg : cfg) : risc_cfg * var_to_reg * reg =
  (** First, build the initial register map, that maps each variable to a register *)
  let initial_reg_map = initial_reg_map cfg.input_var cfg.output_var in
  let all_vars_reg_map =
    SSet.fold
      (fun var acc ->
        if SMap.mem var acc then acc else SMap.add var (fresh_reg ()) acc)
      cfg.all_vars initial_reg_map
  in
  (** Then, allocate temporary registers for the guard and other temporary computations *)
  let guard_reg = fresh_reg () in
  let other_temp_reg = fresh_reg () in
  (** Create a checkpoint for the register allocator, allowing register reuse *)
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

(** Generates the RISC label for a node Id *)
let string_of_label (node_id : int) : string =
  if node_id = 0 then "main" else Printf.sprintf "l%d" node_id

(** Appends a jump instruction representing the outgoing edge of the node:
  - Single outgoing edge: append a unconditional jump
  - Pair of outgoing edges: append a conditional jump based on the guard register
 *)
let append_jump (guard_reg : reg) (cfg : risc_cfg)
    (node_id : int) (instrs : instruction list) : instruction list =
  match IMap.find_opt node_id cfg.edges with
  (* If there is no outgoing edge, the final node has been reached *)
  | None -> instrs
  (* If there is a single outgoing edge, append a jump *)
  | Some (Single next_node) -> instrs @ [ Jump (string_of_label next_node) ]
  (* If there is a pair of outgoing edges,
  append a conditional jump based on the guard register*)
  | Some (Pair (left_node, right_node)) ->
    instrs @ [ CJump (guard_reg, string_of_label left_node, string_of_label right_node) ]

(** Given a RISC CFG, appends jump instructions at the end of the block, 
  representing the node outgoing edges *)
let risc_cfg_with_jumps (guard_reg : reg)
    (risc_cfg : risc_cfg) : risc_cfg =
  let nodes_with_jumps =
    IMap.mapi
      (fun node_id instrs ->
        append_jump guard_reg risc_cfg node_id instrs)
      risc_cfg.nodes
  in
  { risc_cfg with nodes = nodes_with_jumps }

let registers_in_block (instrs : instruction list) : reg_set =
  List.fold_left
    (fun acc instr ->
      let acc =
        match find_defined_reg instr with
        | Some r -> RSet.add r acc
        | None -> acc
      in
      RSet.union acc (find_used_regs instr))
    RSet.empty instrs

let registers_in_cfg (cfg : risc_cfg) : reg_set =
  IMap.fold
    (fun _ instrs acc -> RSet.union acc (registers_in_block instrs))
    cfg.nodes RSet.empty
    
(** Converts a RISC CFG into a RISC valid program *)
let risc_cfg_to_code
    (risc_cfg : risc_cfg) (string_of_instruction : instruction -> string)
    : string =
  List.fold_left
    (fun acc (node_id, _) ->
      let label = string_of_label node_id in
      let instructions =
        IMap.find node_id risc_cfg.nodes
        |> List.map (fun instr -> "\t" ^ string_of_instruction instr)
        |> String.concat "\n"
      in
      acc ^ label ^ ":\n" ^ instructions ^ "\n\n")
    "" (IMap.bindings risc_cfg.nodes)
