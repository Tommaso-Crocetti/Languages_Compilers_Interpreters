open Mini_imp_AST
module SSet = Mini_Modules.SSet

type var_set = Mini_imp_AST.var_set

module IMap = Mini_Modules.IMap

type out_node = Single of int | Pair of int * int

type 'a generic_cfg = {
  nodes : 'a IMap.t;
  edges : out_node IMap.t;
  initial : int;
  final : int list;
  input_var : string;
  output_var : string;
  all_vars : var_set;
}

val empty_cfg : 'a generic_cfg
(** Adds a node to the control flow graph, providing the node Id and node information *)
val add_node : 'a generic_cfg -> int -> 'a -> 'a generic_cfg
(** Adds an edge to the control flow graph *)
val add_edge : 'a generic_cfg -> int -> out_node -> 'a generic_cfg
(** Finds all the predecessors of a given node in the control flow graph *)
val find_predecessors : 'a generic_cfg -> int -> int list
