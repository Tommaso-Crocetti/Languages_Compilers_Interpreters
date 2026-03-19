open Mini_CFG
open Mini_imp_CFG
open Mini_RISC

exception Error of string

module SMap = Mini_RISC.SMap

type risc_cfg = instruction list generic_cfg

(* Main function, translates a CFG to a RISC CFG by traversing the original one *)
let translate_cfg (cfg: cfg): risc_cfg =
  let initial_reg_map = initial_reg_map cfg.input_var cfg.output_var in
  let all_vars_reg_map = SSet.fold (fun var acc ->
    if SMap.mem var acc then acc
    else SMap.add var (fresh_reg ()) acc
  ) cfg.all_vars initial_reg_map in
  let risc_nodes = IMap.mapi (fun node_id (stmts, _) ->
    let instructions, _ = translate_stmts stmts all_vars_reg_map in
    instructions
  ) cfg.nodes in
  { nodes = risc_nodes; edges = cfg.edges; initial = cfg.initial; final = cfg.final; input_var = cfg.input_var; output_var = cfg.output_var; all_vars = cfg.all_vars }

(* Helper function that generates the label for a node Id *)
let string_of_label (node_id: int) : string =
  if node_id = 0 then "main" else Printf.sprintf "l%d" node_id

(* Helper function that appends a jump instruction representing the outgoing edge of the node *)
let append_jump (cfg: risc_cfg) (node_id: int) (instrs: instruction list) : instruction list =
  match IMap.find_opt node_id cfg.edges with
  (* If there is no outgoing edge, the final node has been reached *)
  | None -> instrs
  (* If there is a single outgoing edge, append a jump *)
  | Some (Single next_node) ->
    instrs @ [Jump (string_of_label next_node)]
  (* If there is a pair of outgoing edges,
  append a conditional jump based on the guard register*)
  | Some (Pair (left_node, right_node)) ->
    instrs @ [CJump (Ra, string_of_label left_node, string_of_label right_node)]

let risc_cfg_to_code
  (string_of_instruction : instruction -> string)
  (cfg : risc_cfg)
  : string =
  List.fold_left (fun acc (node_id, (instrs)) ->
    let label = string_of_label node_id in
    let instructions = append_jump cfg node_id instrs in
    acc ^ label ^ ":\n"
    ^ String.concat "\n" (List.map (fun instr -> "\t" ^ string_of_instruction instr) instructions)
    ^ "\n"
  ) "" (IMap.bindings cfg.nodes)