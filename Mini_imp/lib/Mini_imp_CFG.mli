open Mini_imp_AST
open Mini_CFG

exception Error of string

type statement = Skip | Assign of string * a_exp | Guard of b_exp
type cfg = statement list generic_cfg

val fresh_id : unit -> int
val next_node_id : cfg -> int
val add_block : cfg -> statement list -> cfg * int
val connect_pending_node : cfg -> int -> int -> cfg
val build_cfg : program -> cfg
val find_defined_vars : statement list -> var_set
