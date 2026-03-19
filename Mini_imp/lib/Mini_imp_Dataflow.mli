open Mini_imp_AST
open Mini_imp_Parser
open Mini_CFG
open Mini_imp_CFG

exception Error of string

module NMap = Mini_CFG.NMap
module SSet = Mini_CFG.SSet
module ISet : Set.S with type elt = int

type var_set = SSet.t

type dataflow_node =
  {
    stmts: statement list;
    in_vars: var_set;
    out_vars: var_set;
  }

type dataflow_cfg = (dataflow_node * var_set) generic_cfg

val build_dataflow_cfg : cfg -> dataflow_cfg
val find_predecessors : dataflow_cfg -> int -> int list
val defined_local_update : dataflow_cfg -> int -> int list -> var_set * var_set
val defined_global_update : dataflow_cfg -> dataflow_cfg
val verify_node : dataflow_cfg -> int -> bool
val verify_all_nodes : dataflow_cfg -> (int * bool) list
val all_blocks_are_correct : dataflow_cfg -> bool
val defined_analysis : cfg -> dataflow_cfg
