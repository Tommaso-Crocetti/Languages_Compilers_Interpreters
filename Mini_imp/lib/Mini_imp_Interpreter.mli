type var = string

module SMap : Map.S with type key = var

type state 

type a_exp =
  | Aval of int
  | Var of var
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
  | Assign of var * a_exp
  | Seq of command * command
  | If of b_exp * command * command
  | While of b_exp * command

type program = {
  input: var;
  output: var;
  body: command;
}

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

val eval_aexp : a_exp -> state -> int
val eval_bexp : b_exp -> state -> bool
val execute : program -> int -> int

val run_program : program -> unit