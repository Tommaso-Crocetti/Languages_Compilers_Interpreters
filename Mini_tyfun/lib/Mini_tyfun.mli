type var = string

module SMap : Map.S with type key = var

type op

type term
    
type fun_type

type context = fun_type SMap.t

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

val term_type_check: term -> context -> fun_type option
val type_check: term -> fun_type option

val drop_types: term -> Mini_fun.term
val compute_ty: term -> Mini_fun.runtime_value