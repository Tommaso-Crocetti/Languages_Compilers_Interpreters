open Mini_imp_AST
open Mini_CFG
open Mini_imp_CFG

exception Error of string

module IMap = Mini_CFG.IMap
module SSet = Mini_CFG.SSet
module ISet : Set.S with type elt = int

type var_set = Mini_imp_AST.var_set

type dataflow_node =
  {
    stmts: statement list;
    in_vars: var_set;
    out_vars: var_set;
  }

type node_verification_result

type dataflow_cfg = (dataflow_node * var_set) generic_cfg

val build_dataflow_cfg : cfg -> dataflow_cfg
val defined_local_update : dataflow_cfg -> int -> int list -> var_set * var_set
val defined_global_update : dataflow_cfg -> dataflow_cfg
val verify_node : dataflow_cfg -> int -> node_verification_result
val verify_all_nodes : dataflow_cfg -> (int * bool) list
val defined_analysis : cfg -> dataflow_cfg
