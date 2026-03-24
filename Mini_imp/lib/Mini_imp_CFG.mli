open Mini_imp_AST
open Mini_CFG

exception Error of string

(** Statements are valid operations that can be contained in a control flow graph *)
type statement = Skip | Assign of string * a_exp | Guard of b_exp
(** A control flow graph is a generic graph where nodes are associated to lists of statements *)
type cfg = statement list generic_cfg


(** Returns a fresh node Id *)
val fresh_id : unit -> int
(** Returns the next available node Id in the CFG *)
val next_node_id : cfg -> int
(** Add a maximal block to the CFG *)
val add_block : cfg -> statement list -> cfg * int
(** Connects pending nodes to a destination node *)
val connect_pending_node : cfg -> int -> int -> cfg
(* Given a .mimp program, incrementally builds a control flow graph *)
val build_cfg : program -> cfg
(** Finds all variables that are defined in a list of statements *)
val find_defined_vars : statement list -> var_set
