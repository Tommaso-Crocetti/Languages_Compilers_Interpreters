open Mini_imp_AST
open Mini_imp_Parser
open Mini_CFG
open Mini_imp_CFG

module NMap = Mini_CFG.NMap

module SSet = Mini_CFG.SSet

module ISet = Set.Make(Int)

type var_set = SSet.t

type dataflow_node = 
  {
    stmts: statement list;
    in_vars: var_set; (* Variables available at the entry of this statement *)
    out_vars: var_set; (* Variables available at the exit of this statement *)
  }

type dataflow_cfg = (dataflow_node * var_set) generic_cfg

let build_dataflow_cfg (cfg: cfg) : dataflow_cfg =
  let nodes = NMap.map (fun (stmts, def_vars) -> ({ stmts; in_vars = cfg.all_vars; out_vars = cfg.all_vars }, def_vars)) cfg.nodes in
  { nodes; edges = cfg.edges; initial = cfg.initial; final = cfg.final; all_vars = cfg.all_vars; input_var = cfg.input_var; output_var = cfg.output_var }

let string_of_var_set (s: var_set) : string =
  let elems = SSet.elements s in
  "{" ^ String.concat ", " elems ^ "}"

let find_predecessors (df_cfg: dataflow_cfg) (node_id: int) : int list =
  List.filter_map (fun (src_id, out_nodes) ->
      match out_nodes with
      | Single dst_id when dst_id = node_id -> Some src_id
      | Pair (dst_id1, dst_id2) when dst_id1 = node_id || dst_id2 = node_id -> Some src_id
      | _ -> None)
    (NMap.bindings df_cfg.edges)

let defined_local_update (df_cfg: dataflow_cfg) (node_id: int) (predecessors: int list): var_set * var_set =
  let (df_node, def_vars) = NMap.find node_id df_cfg.nodes in
  let new_in_vars =
    if node_id = df_cfg.initial then SSet.singleton df_cfg.input_var
    else
        List.fold_left (fun acc pred_id ->
          let (pred_node, _) = NMap.find pred_id df_cfg.nodes in
          SSet.inter acc pred_node.out_vars) df_cfg.all_vars predecessors      
  in 
  let new_out_vars = SSet.union new_in_vars def_vars in 
  (new_in_vars, new_out_vars)

let defined_global_update (df_cfg: dataflow_cfg) : dataflow_cfg =
  let predecessors_map = NMap.fold (fun node_id _ acc -> NMap.add node_id (find_predecessors df_cfg node_id) acc) df_cfg.nodes NMap.empty in
  let rec visit (df_cfg: dataflow_cfg) (node_id: int) (previous_node: dataflow_node) (visited: ISet.t) : dataflow_cfg * ISet.t =
    let (df_node, def_vars) = NMap.find node_id df_cfg.nodes in
    let (prev_in_vars, prev_out_vars) = (df_node.in_vars, df_node.out_vars) in
    let (new_in_vars, new_out_vars) = defined_local_update df_cfg node_id (NMap.find node_id predecessors_map) in
    if ISet.mem node_id visited && SSet.equal prev_in_vars new_in_vars then (df_cfg, visited) 
    else
      let visited = ISet.add node_id visited in
      let new_node = { df_node with in_vars = new_in_vars; out_vars = new_out_vars } in
      let df_cfg = { df_cfg with nodes = NMap.add node_id (new_node, def_vars) df_cfg.nodes } in
      match NMap.find_opt node_id df_cfg.edges with
      | None -> (df_cfg, visited)
      | Some out_nodes -> 
        (match out_nodes with
        | Single next_id -> visit df_cfg next_id new_node visited
        | Pair (id1, id2) -> 
          let (df_cfg, visited) = visit df_cfg id1 new_node visited in
          visit df_cfg id2 new_node visited
        )
        in let dummy_init = { stmts = []; in_vars = SSet.empty; out_vars = SSet.empty }
        in let (df_cfg, _) = visit df_cfg df_cfg.initial dummy_init ISet.empty in
    df_cfg

let rec get_used_avars (a: a_exp) : var_set =
  match a with
  | Aval _ -> SSet.empty
  | Var x -> SSet.singleton x
  | Of_Bool b -> get_used_bvars b
  | Plus (a1, a2)
  | Minus (a1, a2)
  | Times (a1, a2) ->
    SSet.union (get_used_avars a1) (get_used_avars a2)

and get_used_bvars (b: b_exp) : var_set =
  match b with
  | Bval _ -> SSet.empty
  | And (b1, b2)
  | Or (b1, b2) ->
    SSet.union (get_used_bvars b1) (get_used_bvars b2)
  | Not b1 -> get_used_bvars b1
  | Minor (a1, a2) -> SSet.union (get_used_avars a1) (get_used_avars a2)

let verify_node (df_cfg: dataflow_cfg) (node_id: int) : bool =
  let (df_node, _) = NMap.find node_id df_cfg.nodes in
  let step (acc: var_set option) (stmt: statement) : var_set option =
    match acc with
    | None -> None
    | Some curr_vars ->
      (match stmt with
      | Skip -> Some curr_vars
      | Assign (x, a) ->
        let used_vars = get_used_avars a in
        if SSet.subset used_vars curr_vars then Some (SSet.add x curr_vars) else None
      | Guard b ->
        let used_vars = get_used_bvars b in
        if SSet.subset used_vars curr_vars then Some curr_vars else None)
  in
  match List.fold_left step (Some df_node.in_vars) df_node.stmts with
  | None -> false
  | Some inferred_out_vars -> SSet.equal inferred_out_vars df_node.out_vars

let verify_all_nodes (df_cfg: dataflow_cfg) : (int * bool) list =
  NMap.bindings df_cfg.nodes
  |> List.map (fun (node_id, _) -> (node_id, verify_node df_cfg node_id))

let all_blocks_are_correct (df_cfg: dataflow_cfg) : bool =
  verify_all_nodes df_cfg
  |> List.for_all (fun (_, is_ok) -> is_ok)

let defined_analysis (cfg: cfg) : dataflow_cfg =
  let df_cfg = build_dataflow_cfg cfg in
  let final_df_cfg = defined_global_update df_cfg in 
  let rec fail_on_first_invalid (nodes: (int * (dataflow_node * var_set)) list) : unit =
    match nodes with
    | [] -> ()
    | (node_id, _) :: rest ->
      if verify_node final_df_cfg node_id then fail_on_first_invalid rest
      else failwith ("Dataflow verification failed at block: " ^ string_of_int node_id)
  in
  fail_on_first_invalid (NMap.bindings final_df_cfg.nodes);
  final_df_cfg