type var = string

module SMap : Map.S with type key = var

type op

type term
    
type fun_type

type env = fun_type SMap.t

val plus: op
val minus: op
val times: op

val and_ : op
val or_: op
val minor: op

val int_: int -> term
val bool_: bool -> term
val var : var -> term
val fun_: var -> fun_type -> term -> term
val app : term -> term -> term
val op: op -> term -> term -> term
val not_: term -> term
val if_: term -> term -> term -> term
val let_: var -> term -> term -> term
val letfun: var -> var -> fun_type -> term -> term -> term
