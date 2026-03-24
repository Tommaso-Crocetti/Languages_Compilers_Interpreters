open Mini_CFG
open Mini_RISC
open Mini_RISC_CFG
open Mini_Dataflow
open Mini_imp_Printer

type dataflow_risc_node = {
  instructions : instruction list;
  in_regs : reg_set; 
  out_regs : reg_set;
}
(** A node in the dataflow RISC CFG contains:
    - instructions: the list of RISC instructions in the node
    - in_regs: the set of registers live at the entry of this instruction
    - out_regs: the set of registers live at the exit of this instruction *)

(** A dataflow RISC CFG is a generic CFG where nodes are associated to dataflow RISC nodes *)
type dataflow_risc_cfg = dataflow_risc_node generic_cfg


(** Builds the initial dataflow RISC CFG from a RISC control flow graph, by assigning the
    bottom of the CPO (the empty set of registers) to each in_regs and out_regs *)
let build_dataflow_risc_cfg (cfg : risc_cfg) (guard_reg : reg) :
    dataflow_risc_cfg =
  let cfg_with_jumps = risc_cfg_with_jumps guard_reg cfg in
  let nodes =
    IMap.map
      (fun instructions ->
        { instructions; in_regs = RSet.empty; out_regs = RSet.empty })
      cfg_with_jumps.nodes
  in
  {
    nodes;
    edges = cfg_with_jumps.edges;
    initial = cfg_with_jumps.initial;
    final = cfg_with_jumps.final;
    all_vars = cfg_with_jumps.all_vars;
    input_var = cfg_with_jumps.input_var;
    output_var = cfg_with_jumps.output_var;
  }

(** Performs a local update, redefining the defined in_regs and out_regs of a
    node based on its successors and previously live registers  *)
let liveness_local_update (df_cfg : dataflow_risc_cfg) (node_id : int)
    (used_defined : reg_set * reg_set) : reg_set * reg_set =
  let used_regs, def_regs = used_defined in
  let final_node_id =
    match df_cfg.final with
    | [ n ] -> n
    | _ -> raise (Error "unexpected not single final node")
  in
  let new_out_regs =
    if node_id = final_node_id then RSet.singleton Rout
    else
      let out_node = IMap.find node_id df_cfg.edges in
      match out_node with
      | Single dst_id -> (IMap.find dst_id df_cfg.nodes).in_regs
      | Pair (dst_id1, dst_id2) ->
          RSet.union (IMap.find dst_id1 df_cfg.nodes).in_regs
            (IMap.find dst_id2 df_cfg.nodes).in_regs
  in
  let new_in_regs = RSet.union used_regs (RSet.diff new_out_regs def_regs) in
  (new_in_regs, new_out_regs)

(** Computes the global dataflow analysis exploiting the worklist-based
    algorithm that applies the local update until a fixpoint is reached *)
let liveness_global_update (df_risc_cfg : dataflow_risc_cfg) : dataflow_risc_cfg
    =
    (** Compute the predecessor map, needed to perform a backward visit of the dataflow RISC CFG *)
  let predecessor_map =
    IMap.fold
      (fun node_id _ acc ->
        IMap.add node_id (find_predecessors df_risc_cfg node_id) acc)
      df_risc_cfg.nodes IMap.empty
  in
  (** Compute the map of used and defined registers for each node *)
  let used_defined_map =
    IMap.map
      (fun df_node -> find_used_defined_regs df_node.instructions)
      df_risc_cfg.nodes
  in
  (** Define the initial worklist, containing all nodes in reverse order *)
  let initial_worklist =
    List.rev (List.map fst (IMap.bindings df_risc_cfg.nodes))
  in
  (** Define how to extract the current in_regs and out_regs for a node *)
  let extract_fact (df_node : dataflow_risc_node) : reg_set * reg_set =
    (df_node.in_regs, df_node.out_regs)
  in
  (** Define how to perform the local update for a specific node *)
  let local_update (df_cfg : dataflow_risc_cfg) (node_id : int) :
      reg_set * reg_set =
    let used_defined = IMap.find node_id used_defined_map in
    liveness_local_update df_cfg node_id used_defined
  in
  (** Define the equality function for comparing register sets *)
  let eq (prev_regs : reg_set * reg_set) (new_regs : reg_set * reg_set) : bool =
    let _, prev_out_regs = prev_regs in
    let _, new_out_regs = new_regs in
    RSet.equal prev_out_regs new_out_regs
  in
  (** Define how to update a node with new in_regs and out_regs *)
  let update_node (df_cfg : dataflow_risc_cfg) (df_node : dataflow_risc_node)
      (new_in_out : reg_set * reg_set) : dataflow_risc_node =
    let new_in_regs, new_out_regs = new_in_out in
    { df_node with in_regs = new_in_regs; out_regs = new_out_regs }
  in
  (** Define the list of successors for a given node, to perform the visit *)
  let successors (node_id : int) : int list =
    IMap.find node_id predecessor_map
  in
  (** Define how to update the worklist with new nodes to visit *)
  let update_worklist (worklist : int list) (node_id : int)
      (successors : int list) : int list =
    List.filter (fun id -> id <> node_id) worklist @ successors
  in
  (** Compute the fixpoint for the liveness analysis *)
  visit df_risc_cfg initial_worklist extract_fact local_update eq update_node
    successors update_worklist

let print_in_out_regs (df_cfg : dataflow_risc_cfg) : unit =
  Printf.printf "Liveness Analysis Results:\n";
  IMap.iter
    (fun node_id df_node ->
      let in_regs_str =
        RSet.elements df_node.in_regs
        |> List.map string_of_risc_reg
        |> String.concat ", "
      in
      let out_regs_str =
        RSet.elements df_node.out_regs
        |> List.map string_of_risc_reg
        |> String.concat ", "
      in
      print_endline
        ("Node " ^ string_of_int node_id ^ ": InRegs = {" ^ in_regs_str
       ^ "}, OutRegs = {" ^ out_regs_str ^ "}"))
    df_cfg.nodes
