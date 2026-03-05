type var = string

module SMap : Map.S with type key = var

type op

type term
    
type runtime_value = 
    | IntV of int
    | BoolV of bool
    | ClosureV of var * term * env
    | RecClosureV of var * var * term * env
and env = runtime_value SMap.t

val plus: op
val minus: op
val times: op

val and_ : op
val or_: op
val minor: op

val int_: int -> term
val bool_: bool -> term
val var : var -> term
val fun_: var -> term -> term
val app : term -> term -> term
val op: op -> term -> term -> term
val not_: term -> term
val if_: term -> term -> term -> term
val let_: var -> term -> term -> term
val letfun: var -> var -> term -> term -> term


val eval_t: term -> env -> runtime_value
