type var = string

module SMap : Map.S with type key = var

type state = int SMap.t

type a_exp
type b_exp
type command
type program

val aval : int -> a_exp
val var : var -> a_exp
val plus : a_exp -> a_exp -> a_exp
val minus : a_exp -> a_exp -> a_exp
val times : a_exp -> a_exp -> a_exp
val of_bool : b_exp -> a_exp

val bval : bool -> b_exp
val and_ : b_exp -> b_exp -> b_exp
val or_ : b_exp -> b_exp -> b_exp
val not_ : b_exp -> b_exp
val minor : a_exp -> a_exp -> b_exp

val skip : command
val assign : var -> a_exp -> command
val seq : command -> command -> command
val if_ : b_exp -> command -> command -> command
val while_ : b_exp -> command -> command

val make_program : var -> var -> command -> program

val eval_a : a_exp -> state -> int
val eval_b : b_exp -> state -> bool
val eval_c : command -> state -> state
val execute : program -> int -> int