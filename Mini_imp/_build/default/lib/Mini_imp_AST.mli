exception Error of string

type a_exp =
  | Aval of int
  | Var of string
  | Of_Bool of b_exp
  | Plus of a_exp * a_exp
  | Minus of a_exp * a_exp
  | Times of a_exp * a_exp

and b_exp =
  | Bval of bool
  | And of b_exp * b_exp
  | Or of b_exp * b_exp
  | Not of b_exp
  | Minor of a_exp * a_exp

type command =
  | Skip
  | Assign of string * a_exp
  | Seq of command * command
  | If of b_exp * command * command
  | While of b_exp * command

type program = { input_var : string; output_var : string; body : command }
type state
type var_set = Mini_Modules.SSet.t

val make_program : string -> string -> command -> program
val eval_aexp : a_exp -> state -> int
val eval_bexp : b_exp -> state -> bool
val eval_c : command -> state -> state
val execute : program -> int -> int
(** Finds all variables in an arithmetic expression *)
val find_all_vars_aexp : a_exp -> var_set
(** Finds all variables in a boolean expression *)
val find_all_vars_bexp : b_exp -> var_set
(** Finds all variables in a command *)
val find_all_vars_command : command -> var_set
