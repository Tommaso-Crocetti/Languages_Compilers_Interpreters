type var = string

module SMap : Map.S with type key = var

type op
and term
    
type runtime_value
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
val compute: term -> runtime_value