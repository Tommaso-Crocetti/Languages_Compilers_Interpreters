open Mini_imp_AST
open Mini_CFG
open Mini_imp_CFG
open Mini_Dataflow
open Mini_imp_Printer

exception Error of string

module IMap = Mini_Modules.IMap
module SSet = Mini_Modules.SSet

type var_set = Mini_imp_AST.var_set

type dataflow_node = {
  stmts : statement list;
  in_vars : var_set;
  out_vars : var_set;
}
(** A node in the dataflow graph contains:
    - stmts: the list of statements in the node
    - in_vars: the set of variables available at the entry of this statement
    - out_vars: the set of variables available at the exit of this statement *)

type dataflow_cfg = dataflow_node generic_cfg

(** The result of verifying a node in the dataflow graph:
    - NodeOk: the node is verified successfully
    - UndefinedVariables: the node contains undefined variables
    - OutVarsMismatch: there is a mismatch between annotated and inferred output
      variables *)
type node_verification_result =
  | NodeOk
  | UndefinedVariables of { stmt : statement; undefined_vars : var_set }
  | OutVarsMismatch of {
      annotated_out_vars : var_set;
      inferred_out_vars : var_set;
    }

(** Builds the initial dataflow CFG from a control flow graph, by assigning the
    top of the CPO (the set of all variables) to each in_vars and out_vars *)
let build_dataflow_cfg (cfg : cfg) : dataflow_cfg =
  let nodes =
    IMap.map
      (fun stmts -> { stmts; in_vars = cfg.all_vars; out_vars = cfg.all_vars })
      cfg.nodes
  in
  {
    nodes;
    edges = cfg.edges;
    initial = cfg.initial;
    final = cfg.final;
    all_vars = cfg.all_vars;
    input_var = cfg.input_var;
    output_var = cfg.output_var;
  }

(** Performs a local update, redefining the defined in_vars and out_vars of a
    node based on its predecessors and previously defined variables *)
let defined_local_update (df_cfg : dataflow_cfg) (node_id : int)
    (predecessors : int list) (def_vars : var_set) : var_set * var_set =
  let new_in_vars =
    if node_id = df_cfg.initial then SSet.singleton df_cfg.input_var
    else
      List.fold_left
        (fun acc pred_id ->
          let pred_node = IMap.find pred_id df_cfg.nodes in
          SSet.inter acc pred_node.out_vars)
        df_cfg.all_vars predecessors
  in
  let new_out_vars = SSet.union new_in_vars def_vars in
  (new_in_vars, new_out_vars)

(** Computes the global dataflow analysis exploiting the worklist-based
    algorithm that applies the local update until a fixpoint is reached *)
let defined_global_update (df_cfg : dataflow_cfg) : dataflow_cfg =
  (** Compute the predecessor map, needed to propagate local updates *)
  let predecessors_map =
    IMap.fold
      (fun node_id _ acc ->
        IMap.add node_id (find_predecessors df_cfg node_id) acc)
      df_cfg.nodes IMap.empty
  in
  (** Compute the map of defined variables for each node *)
  let def_vars_map =
    IMap.map (fun df_node -> find_defined_vars df_node.stmts) df_cfg.nodes
  in
  (** Define the initial worklist, containing all nodes in order *)
  let initial_worklist = List.map fst (IMap.bindings df_cfg.nodes) in
  (** Define how to extract the current in_vars and out_vars for a node *)
  let extract_fact (df_node : dataflow_node) : var_set * var_set =
    (df_node.in_vars, df_node.out_vars)
  in
  (** Define how to perform the local update for a specific node *)
  let local_update (df_cfg : dataflow_cfg) (node_id : int) : var_set * var_set =
    let predecessors = IMap.find node_id predecessors_map in
    let def_vars = IMap.find node_id def_vars_map in
    defined_local_update df_cfg node_id predecessors def_vars
  in
  (** Define the equality function for comparing variable sets *)
  let eq (prev_vars : var_set * var_set) (new_vars : var_set * var_set) : bool =
    let prev_in_vars, _ = prev_vars in
    let in_vars2, _ = new_vars in
    SSet.equal prev_in_vars in_vars2
  in
  (** Define how to update a node with new in_vars and out_vars *)
  let update_node (df_cfg : dataflow_cfg) (df_node : dataflow_node)
      (new_in_out : var_set * var_set) : dataflow_node =
    let new_in_vars, new_out_vars = new_in_out in
    { df_node with in_vars = new_in_vars; out_vars = new_out_vars }
  in
  (** Define the list of successors for a given node, to perform the visit *)
  let successors (node_id : int) : int list =
    match IMap.find_opt node_id df_cfg.edges with
    | None -> []
    | Some out_nodes -> (
        match out_nodes with
        | Single next_id -> [ next_id ]
        | Pair (id1, id2) -> [ id1; id2 ])
  in
  (** Define how to update the worklist with new nodes to visit *)
  let update_worklist (worklist : int list) (node_id : int)
      (successors : int list) : int list =
    List.filter (fun id -> id <> node_id) worklist @ successors
  in
  (** Compute the fixpoint for the defined variables analysis *)
  visit df_cfg initial_worklist extract_fact local_update eq update_node
    successors update_worklist

(** Verifies a node in the dataflow graph, by checking if each instruction uses
    previously defined variables *)
let verify_node (df_cfg : dataflow_cfg) (node_id : int) :
    node_verification_result =
  let df_node = IMap.find node_id df_cfg.nodes in
  let rec step (curr_vars : var_set) (stmts : statement list) :
      node_verification_result =
    match stmts with
    | [] ->
        if SSet.equal curr_vars df_node.out_vars then NodeOk
        else
          OutVarsMismatch
            {
              annotated_out_vars = df_node.out_vars;
              inferred_out_vars = curr_vars;
            }
    | stmt :: rest -> (
        match stmt with
        | Skip -> step curr_vars rest
        | Assign (x, a) ->
            let used_vars = find_all_vars_aexp a in
            if SSet.subset used_vars curr_vars then
              step (SSet.add x curr_vars) rest
            else
              UndefinedVariables
                { stmt; undefined_vars = SSet.diff used_vars curr_vars }
        | Guard b ->
            let used_vars = find_all_vars_bexp b in
            if SSet.subset used_vars curr_vars then step curr_vars rest
            else
              UndefinedVariables
                { stmt; undefined_vars = SSet.diff used_vars curr_vars })
  in
  step df_node.in_vars df_node.stmts

(** Verifies if all nodes in the dataflow graph satisfy the defined variables
    analysis *)
let verify_all_nodes (df_cfg : dataflow_cfg) : (int * bool) list =
  let verify_nodes =
    List.map
      (fun (node_id, _) -> (node_id, verify_node df_cfg node_id))
      (IMap.bindings df_cfg.nodes)
  in
  match List.find_opt (fun (_, status) -> status <> NodeOk) verify_nodes with
  | Some (node_id, status) -> (
      match status with
      | NodeOk -> raise (Error "Unexpected NodeOk status in verify_all_nodes")
      | UndefinedVariables { stmt; undefined_vars } ->
          let undefined_vars_str =
            String.concat ", " (SSet.elements undefined_vars)
          in
          raise
            (Error
               (Printf.sprintf
                  "node %d has undefined variables {%s} in statement %s" node_id
                  undefined_vars_str
                  (string_of_cfg_statement stmt)))
      | OutVarsMismatch { annotated_out_vars; inferred_out_vars } ->
          let annotated_out_vars_str =
            String.concat ", " (SSet.elements annotated_out_vars)
          in
          let inferred_out_vars_str =
            String.concat ", " (SSet.elements inferred_out_vars)
          in
          raise
            (Error
               (Printf.sprintf
                  "node %d has out_vars mismatch: annotated out_vars = {%s}, \
                   but inferred out_vars = {%s}"
                  node_id annotated_out_vars_str inferred_out_vars_str)))
  | None ->
      List.map
        (fun (node_id, _) -> (node_id, true))
        (IMap.bindings df_cfg.nodes)

(** Globally performs the defined variables analysis on a control flow graph *)
let defined_analysis (cfg : cfg) : dataflow_cfg =
  let df_cfg = build_dataflow_cfg cfg in
  let final_df_cfg = defined_global_update df_cfg in
  let _ = verify_all_nodes final_df_cfg in
  final_df_cfg
