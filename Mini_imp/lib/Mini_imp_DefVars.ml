open Mini_imp_AST
open Mini_CFG
open Mini_imp_CFG
open Mini_imp_Printer

exception Error of string

module IMap = Mini_Modules.IMap

module SSet = Mini_Modules.SSet

module ISet = Mini_Modules.ISet

type var_set = Mini_imp_AST.var_set

type dataflow_node = 
  {
    stmts: statement list;
    in_vars: var_set; (* Variables available at the entry of this statement *)
    out_vars: var_set; (* Variables available at the exit of this statement *)
  }

type dataflow_cfg = (dataflow_node * var_set) generic_cfg

type node_verification_result =
  | NodeOk
  | UndefinedVariables of { stmt: statement; undefined_vars: var_set }
  | OutVarsMismatch of { annotated_out_vars: var_set; inferred_out_vars: var_set }

let build_dataflow_cfg (cfg: cfg) : dataflow_cfg =
  let nodes = IMap.map (fun (stmts, def_vars) -> ({ stmts; in_vars = cfg.all_vars; out_vars = cfg.all_vars }, def_vars)) cfg.nodes in
  { nodes; edges = cfg.edges; initial = cfg.initial; final = cfg.final; all_vars = cfg.all_vars; input_var = cfg.input_var; output_var = cfg.output_var }

let defined_local_update (df_cfg: dataflow_cfg) (node_id: int) (predecessors: int list): var_set * var_set =
  let (df_node, def_vars) = IMap.find node_id df_cfg.nodes in
  let new_in_vars =
    if node_id = df_cfg.initial then SSet.singleton df_cfg.input_var
    else
        List.fold_left (fun acc pred_id ->
          let (pred_node, _) = IMap.find pred_id df_cfg.nodes in
          SSet.inter acc pred_node.out_vars) df_cfg.all_vars predecessors      
  in 
  let new_out_vars = SSet.union new_in_vars def_vars in 
  (new_in_vars, new_out_vars)

let defined_global_update (df_cfg: dataflow_cfg) : dataflow_cfg =
  let predecessors_map = IMap.fold (fun node_id _ acc -> IMap.add node_id (find_predecessors df_cfg node_id) acc) df_cfg.nodes IMap.empty in
  let rec visit (df_cfg: dataflow_cfg) (node_id: int) (visited: ISet.t) : dataflow_cfg * ISet.t =
    let (df_node, def_vars) = IMap.find node_id df_cfg.nodes in
    let (prev_in_vars, prev_out_vars) = (df_node.in_vars, df_node.out_vars) in
    let (new_in_vars, new_out_vars) = defined_local_update df_cfg node_id (IMap.find node_id predecessors_map) in
    if ISet.mem node_id visited && SSet.equal prev_in_vars new_in_vars then (df_cfg, visited) 
    else
      let visited = ISet.add node_id visited in
      let new_node = { df_node with in_vars = new_in_vars; out_vars = new_out_vars } in
      let df_cfg = { df_cfg with nodes = IMap.add node_id (new_node, def_vars) df_cfg.nodes } in
      match IMap.find_opt node_id df_cfg.edges with
      | None -> (df_cfg, visited)
      | Some out_nodes -> 
        (match out_nodes with
        | Single next_id -> visit df_cfg next_id visited
        | Pair (id1, id2) -> 
          let (df_cfg, visited) = visit df_cfg id1 visited in
          visit df_cfg id2 visited
        )
        in let (df_cfg, _) = visit df_cfg df_cfg.initial ISet.empty in
    df_cfg

let verify_node (df_cfg: dataflow_cfg) (node_id: int) : node_verification_result =
  let (df_node, _) = IMap.find node_id df_cfg.nodes in
  let rec step (curr_vars: var_set) (stmts: statement list) : node_verification_result =
    match stmts with
    | [] ->
      if SSet.equal curr_vars df_node.out_vars then NodeOk
      else
        OutVarsMismatch { annotated_out_vars = df_node.out_vars; inferred_out_vars = curr_vars }
    | stmt :: rest ->
      (match stmt with
      | Skip -> step curr_vars rest
      | Assign (x, a) ->
        let used_vars = find_all_vars_aexp a in
        if SSet.subset used_vars curr_vars then
          step (SSet.add x curr_vars) rest
        else
          UndefinedVariables { stmt; undefined_vars = SSet.diff used_vars curr_vars }
      | Guard b ->
        let used_vars = find_all_vars_bexp b in
        if SSet.subset used_vars curr_vars then
          step curr_vars rest
        else
          UndefinedVariables { stmt; undefined_vars = SSet.diff used_vars curr_vars })
  in
  step df_node.in_vars df_node.stmts

let verify_all_nodes (df_cfg: dataflow_cfg) : (int * bool) list =
  let verify_nodes = List.map (fun (node_id, _) -> (node_id, verify_node df_cfg node_id)) (IMap.bindings df_cfg.nodes) in
  match List.find_opt (fun (_, status) -> status <> NodeOk) verify_nodes with
  | Some (node_id, status) -> 
    (match status with
    | NodeOk -> assert false
    | UndefinedVariables { stmt; undefined_vars } ->
      let undefined_vars_str = String.concat ", " (SSet.elements undefined_vars) in
      raise (Error (Printf.sprintf "node %d has undefined variables {%s} in statement %s" node_id undefined_vars_str (string_of_cfg_statement stmt)))
    | OutVarsMismatch { annotated_out_vars; inferred_out_vars } ->
      let annotated_out_vars_str = String.concat ", " (SSet.elements annotated_out_vars) in
      let inferred_out_vars_str = String.concat ", " (SSet.elements inferred_out_vars) in
      raise (Error (Printf.sprintf "node %d has out_vars mismatch: annotated out_vars = {%s}, but inferred out_vars = {%s}" node_id annotated_out_vars_str inferred_out_vars_str))
    )
  | None -> List.map (fun (node_id, _) -> (node_id, true)) (IMap.bindings df_cfg.nodes)

let defined_analysis (cfg: cfg) : dataflow_cfg =
  let df_cfg = build_dataflow_cfg cfg in
  let final_df_cfg = defined_global_update df_cfg in 
  let _ = verify_all_nodes final_df_cfg in
  final_df_cfg