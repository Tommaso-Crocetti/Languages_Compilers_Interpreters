open Mini_imp_AST
open Mini_CFG
open Mini_imp_CFG

exception Error of string

module IMap = Mini_CFG.IMap
module SSet = Mini_CFG.SSet

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

type dataflow_cfg = dataflow_node generic_cfg

val build_dataflow_cfg : cfg -> dataflow_cfg
(** Builds the initial dataflow CFG from a control flow graph, by assigning the
    top of the CPO (the set of all variables) to each in_vars and out_vars *)

(** Performs a local update, redefining the defined in_vars and out_vars of a
    node based on its predecessors and previously defined variables *)
val defined_local_update :
  dataflow_cfg -> int -> int list -> var_set -> var_set * var_set
(** Computes the global dataflow analysis exploiting the worklist-based
    algorithm that applies the local update until a fixpoint is reached *)
val defined_global_update : dataflow_cfg -> dataflow_cfg
(** Verifies a node in the dataflow graph, by checking if each instruction uses
    previously defined variables *)
val verify_node : dataflow_cfg -> int -> node_verification_result
(** Verifies if all nodes in the dataflow graph satisfy the defined variables
    analysis *)
val verify_all_nodes : dataflow_cfg -> (int * bool) list
(** Globally performs the defined variables analysis on a control flow graph *)
val defined_analysis : cfg -> dataflow_cfg
