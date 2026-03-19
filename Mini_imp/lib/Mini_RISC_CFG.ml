open Mini_CFG
open Mini_imp_CFG
open Mini_RISC

exception Error of string

module SMap = Mini_RISC.SMap

module ISet = Set.Make(Int)

type risc_cfg = instruction list generic_cfg

(* Main function, translates a CFG to a RISC CFG by traversing the original one *)
let translate_cfg (g: cfg): risc_cfg =
  let initial_reg_map = initial_reg_map g.input_var g.output_var in
  let all_vars_reg_map = SSet.fold (fun var acc ->
    if SMap.mem var acc then acc
    else SMap.add var (fresh_reg ()) acc
  ) g.all_vars initial_reg_map in 
  let rec rec_translate_cfg
    (cfg: cfg)
    (risc_cfg: risc_cfg)
    (node_id: int)  (* Current node Id *)
    (visited: ISet.t)   (* Set of visited nodes *)
    (curr_reg_map: var_to_reg)  (* Current register map *)
    (stop_before: int option)  (* Optional node Id to stop before, used for managing join nodes *)
    : risc_cfg * ISet.t * var_to_reg =
    (* Upon reaching the stop node or an already visited node, stop the visit *)
    if stop_before = Some node_id || ISet.mem node_id visited then (risc_cfg, visited, curr_reg_map)
    else
      (* Add the current node to the visited set and translate the block *)
      let visited = ISet.add node_id visited in
      let (stmts, _) = IMap.find node_id cfg.nodes in
      let instructions, next_reg_map = translate_stmts stmts curr_reg_map in
      (* Add the corresponding RISC node and the edges to the RISC CFG *)
      let risc_cfg = add_node risc_cfg node_id instructions in
      match IMap.find_opt node_id cfg.edges with
      (* If there is no outgoing edge, the final node has been reached *)
      | None -> (risc_cfg, visited, next_reg_map)
      | Some out_edge ->
        let risc_cfg = add_edge risc_cfg node_id out_edge in
        (match out_edge with
        (* If the outgoing edge point to a single node, sequentially continue the visit *)
        | Single next_node ->
          rec_translate_cfg cfg risc_cfg next_node visited next_reg_map stop_before
        (* If the outgoing edge is a pair of nodes, visit the left branch until the
        first node of the right branch, then continue from the right branch. *)
        | Pair (left_node, right_node) ->
          (* First visit the left node, either the initial node of a then branch or of a while body *)
          let (risc_cfg, visited, next_reg_map) = 
            rec_translate_cfg cfg risc_cfg left_node visited next_reg_map (Some right_node) in
          (* Then visit the right node, that is the initial node of the else branch
          or the while continuation *)
          rec_translate_cfg cfg risc_cfg right_node visited next_reg_map stop_before
        )
  in
  let final_node =
    match g.final with
    | [] -> g.initial
    | [n] -> n
    | _ -> raise (Error "multiple final nodes not expected")
  in
  let initial_risc_cfg = 
    { empty_cfg with initial = g.initial; 
    final = [final_node]; 
    input_var = g.input_var; 
    output_var = g.output_var 
  } in
  let (risc_cfg, _, _) = rec_translate_cfg g initial_risc_cfg g.initial ISet.empty all_vars_reg_map None in
  risc_cfg

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