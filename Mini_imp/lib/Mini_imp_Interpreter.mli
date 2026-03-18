open Mini_imp_AST

type state 

val make_program : string -> string -> command -> program

val eval_aexp : a_exp -> state -> int
val eval_bexp : b_exp -> state -> bool
val execute : program -> int -> int

val run_program : program -> unit