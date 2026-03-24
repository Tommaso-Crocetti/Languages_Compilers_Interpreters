open Mini_CFG

let rec visit (cfg : 'node generic_cfg) (working_list : int list)
    (extract_fact : 'node -> 'set * 'set)
    (local_update : 'node generic_cfg -> int -> 'set * 'set)
    (eq : 'set * 'set -> 'set * 'set -> bool)
    (update_node : 'node generic_cfg -> 'node -> 'set * 'set -> 'node)
    (successors : int -> int list)
    (update_worklist : int list -> int -> int list -> int list) :
    'node generic_cfg =
  match working_list with
  | [] -> cfg
  | node_id :: rest ->
      let node = IMap.find node_id cfg.nodes in
      let prev_set1, prev_set2 = extract_fact node in
      let new_set1, new_set2 = local_update cfg node_id in
      if eq (prev_set1, prev_set2) (new_set1, new_set2) then
        visit cfg rest extract_fact local_update eq update_node successors
          update_worklist
      else
        let updated_node = update_node cfg node (new_set1, new_set2) in
        let cfg' =
          { cfg with nodes = IMap.add node_id updated_node cfg.nodes }
        in
        let worklist' = update_worklist rest node_id (successors node_id) in
        visit cfg' worklist' extract_fact local_update eq update_node successors
          update_worklist
