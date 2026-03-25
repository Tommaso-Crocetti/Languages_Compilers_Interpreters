open Mini_fun

type var = string

module SMap : Map.S with type key = var

type fun_type =
  | IntT
  | BoolT
  | ArrowT of fun_type * fun_type

type op =
  | Plus
  | Minus
  | Times
  | And
  | Or
  | Minor

type term =
  | Int of int
  | Bool of bool
  | Var of var
  | Fun of var * fun_type * term
  | App of term * term
  | Op of op * term * term
  | Not of term
  | If of term * term * term
  | Let of var * term * term
  | LetFun of var * var * fun_type * term * term

type context = fun_type SMap.t

val type_check: term -> fun_type option

val drop_types: term -> Mini_fun.term

val type_to_string: fun_type -> string
val ast_to_string: term -> string
