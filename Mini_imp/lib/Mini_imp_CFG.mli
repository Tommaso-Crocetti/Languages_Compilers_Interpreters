open Mini_imp_Parser
open Mini_imp_AST
open Mini_CFG

exception Error of string

type statement =
  | Skip
  | Assign of string * a_exp
  | Guard of b_exp

type cfg = (statement list * var_set) generic_cfg

val fresh_id : unit -> int
val empty_cfg : cfg
val add_node : cfg -> statement list -> var_set -> cfg
val add_edge : cfg -> int -> out_node -> cfg
val next_node_id : cfg -> int
val connect_pending_node : cfg -> int -> int -> cfg
val make_cfg : program -> cfg
