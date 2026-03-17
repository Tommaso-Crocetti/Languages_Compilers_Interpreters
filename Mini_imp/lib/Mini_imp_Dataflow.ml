module NMap = Mini_imp_CFG.NMap

module SSet = Mini_imp_CFG.SSet

type var_set = SSet.t

type data_flow_node = 
  {
    stmts: Mini_imp_CFG.statement list;
    in_vars: var_set; (* Variables available at the entry of this statement *)
    out_vars: var_set; (* Variables available at the exit of this statement *)
  }

type dataflow_cfg = 
  {
    nodes: data_flow_node NMap.t;
    edges: Mini_imp_CFG.out_node NMap.t;
    initial: int;
    final: int list;
    defined_vars: var_set;
    input_var: string;
    output_var: string
  }

let build_dataflow_cfg (cfg: Mini_imp_CFG.cfg) : dataflow_cfg =
  let nodes = NMap.map (fun stmts -> { stmts; in_vars = cfg.defined_vars; out_vars = cfg.defined_vars }) cfg.nodes in
  { nodes; edges = cfg.edges; initial = cfg.initial; final = cfg.final; defined_vars = cfg.defined_vars; input_var = cfg.input_var; output_var = cfg.output_var }

let find_defined_vars (stmts: Mini_imp_CFG.statement list) : var_set =
  List.fold_left (fun acc stmt ->
    match stmt with
    | Mini_imp_CFG.Assign (var, _) -> SSet.add var acc
    | _ -> acc
  ) SSet.empty stmts

let defined_local_update (df_cfg: dataflow_cfg) (node_id: int) (previous_node: Mini_imp_CFG.out_node): var_set * var_set =
  let node = NMap.find node_id df_cfg.nodes in
  let new_out_vars = SSet.union node.in_vars (find_defined_vars node.stmts) in
  let new_in_vars =
    if node_id = df_cfg.initial then SSet.singleton df_cfg.input_var
    else
      match previous_node with
      | Mini_imp_CFG.Single prev_id ->
          let prev_node = NMap.find prev_id df_cfg.nodes in
          prev_node.out_vars
      | Mini_imp_CFG.Pair (prev_id1, prev_id2) ->
          let prev_node1 = NMap.find prev_id1 df_cfg.nodes in
          let prev_node2 = NMap.find prev_id2 df_cfg.nodes in
          SSet.inter prev_node1.out_vars prev_node2.out_vars
  in
  (new_in_vars, new_out_vars)
