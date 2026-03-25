type var = string

module SMap : Map.S with type key = var

type op =
  | Plus
  | Minus
  | Times
  | And
  | Or
  | Minor

and term =
  | Int of int
  | Bool of bool
  | Var of var
  | Fun of var * term
  | App of term * term
  | Op of op * term * term
  | Not of term
  | If of term * term * term
  | Let of var * term * term
  | LetFun of var * var * term * term

type runtime_value =
  | IntV of int
  | BoolV of bool
  | ClosureV of var * term * env
  | RecClosureV of var * var * term * env

and env = runtime_value SMap.t

val eval_t: term -> env -> runtime_value
val compute: term -> runtime_value

val extract_int: runtime_value -> int
