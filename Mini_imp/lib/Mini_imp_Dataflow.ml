module NMap = Mini_imp_CFG.NMap

module SSet = Mini_imp_CFG.SSet

type var_set = SSet.t

type dataflow_node = 
  {
    stmts: Mini_imp_CFG.statement list;
    in_vars: var_set; (* Variables available at the entry of this statement *)
    out_vars: var_set; (* Variables available at the exit of this statement *)
  }

type dataflow_cfg = (dataflow_node * var_set) Mini_imp_CFG.generic_cfg

let build_dataflow_cfg (cfg: Mini_imp_CFG.cfg) : dataflow_cfg =
  let nodes = NMap.map (fun (stmts, def_vars) -> ({ stmts; in_vars = cfg.all_vars; out_vars = cfg.all_vars }, def_vars)) cfg.nodes in
  { nodes; edges = cfg.edges; initial = cfg.initial; final = cfg.final; all_vars = cfg.all_vars; input_var = cfg.input_var; output_var = cfg.output_var }

let defined_local_update (df_cfg: dataflow_cfg) (node_id: int) (previous_node: dataflow_node): var_set * var_set =
  let (df_node, def_vars) = NMap.find node_id df_cfg.nodes in
  let new_in_vars =
    if node_id = df_cfg.initial then SSet.singleton df_cfg.input_var
    else
      SSet.inter previous_node.out_vars df_node.in_vars
  in 
  let new_out_vars = SSet.union new_in_vars def_vars in 
  (new_in_vars, new_out_vars)
