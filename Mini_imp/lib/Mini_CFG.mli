open Mini_imp_AST

module SSet : Set.S with type elt = String.t

type var_set = SSet.t

module NMap : Map.S with type key = int

type out_node =
  | Single of int
  | Pair of int * int

type 'a generic_cfg =
  {
    nodes: 'a NMap.t;
    edges: out_node NMap.t;
    initial: int;
    final: int list;
    input_var: string;
    output_var: string;
    all_vars: var_set;
  }

val empty_generic_cfg : 'a generic_cfg

val generic_add_node : 'a generic_cfg -> int -> 'a -> 'a generic_cfg
val generic_add_edge : 'a generic_cfg -> int -> out_node -> 'a generic_cfg

val find_all_vars_aexp : a_exp -> var_set
val find_all_vars_bexp : b_exp -> var_set
val find_all_vars_command : command -> var_set
