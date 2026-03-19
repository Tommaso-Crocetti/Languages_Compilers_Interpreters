open Mini_CFG
open Mini_RISC
open Mini_RISC_CFG

type dataflow_risc_node =
  {
    instructions: instruction list;
    in_regs: reg_set; (* Registers live at the entry of this instruction *)
    out_regs: reg_set; (* Registers live at the exit of this instruction *)
  }

type dataflow_risc_cfg = dataflow_risc_node generic_cfg

let build_dataflow_risc_cfg (cfg: risc_cfg) : dataflow_risc_cfg =
	let nodes = IMap.map (fun instructions -> { instructions; in_regs = RSet.empty; out_regs = RSet.empty }) cfg.nodes in
	{ nodes; edges = cfg.edges; initial = cfg.initial; final = cfg.final; all_vars = cfg.all_vars; input_var = cfg.input_var; output_var = cfg.output_var }

let liveness_local_update (df_cfg: dataflow_risc_cfg) (node_id: int) (used_defined: reg_set * reg_set): reg_set * reg_set =
	let (used_regs, def_regs) = used_defined in
	let final_node_id = 
		match df_cfg.final with
		| [n] -> n
		| _ -> raise (Error ("unexpected not single final node"))
	in 
	let new_out_regs = if node_id = final_node_id then RSet.singleton Rout
	else 
		let out_node = IMap.find node_id df_cfg.edges in
		match out_node with
		| Single dst_id -> (IMap.find dst_id df_cfg.nodes).in_regs
		| Pair (dst_id1, dst_id2) -> RSet.union (IMap.find dst_id1 df_cfg.nodes).in_regs (IMap.find dst_id2 df_cfg.nodes).in_regs
	in 
	let new_in_regs = RSet.union (used_regs) (RSet.diff new_out_regs def_regs) in
  (new_in_regs, new_out_regs)

let liveness_global_update (df_risc_cfg: dataflow_risc_cfg) : dataflow_risc_cfg =
	let predecessor_map = IMap.fold (fun node_id _ acc -> IMap.add node_id (find_predecessors df_risc_cfg node_id) acc) df_risc_cfg.edges IMap.empty in
	let used_defined_map = IMap.map (fun df_node -> find_used_defined_regs df_node.instructions) df_risc_cfg.nodes in
	let rec visit (df_risc_cfg: dataflow_risc_cfg) (working_list: int list) : dataflow_risc_cfg * int list =
		match working_list with
		| [] -> (df_risc_cfg, [])
		| node_id :: rest ->
			let df_node = IMap.find node_id df_risc_cfg.nodes in
			let (prev_in_regs, prev_out_regs) = (df_node.in_regs, df_node.out_regs) in
			let (predecessors, used_defined) = (IMap.find node_id predecessor_map, IMap.find node_id used_defined_map) in
			let (new_in_regs, new_out_regs) = liveness_local_update df_risc_cfg node_id used_defined in
			if RSet.equal prev_out_regs new_out_regs then
				visit df_risc_cfg rest
			else
				let updated_node = { df_node with in_regs = new_in_regs; out_regs = new_out_regs } in
				let updated_cfg = { df_risc_cfg with nodes = IMap.add node_id updated_node df_risc_cfg.nodes } in
				let new_working_list = List.filter (fun id -> id <> node_id) rest @ predecessors in
				visit updated_cfg new_working_list
	in let all_node_ids = List.map fst (IMap.bindings df_risc_cfg.nodes) 
	in let (final_cfg, _) = visit df_risc_cfg (List.rev all_node_ids) in
	final_cfg
