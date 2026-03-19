open Mini_imp_AST
open Mini_CFG
open Mini_imp_CFG

exception Error of string

type reg = Rin | Rout | Ra | Rb | RVar of int

module SMap = Mini_Modules.SMap

type var_to_reg = reg SMap.t

type reg_set

type brop = Add | Sub | Mult | And | Or | Less
type biop = AddI | SubI | MultI | AndI | OrI
type urop = Not | Copy

type instruction =
  | Nop
  | Brop of brop * reg * reg * reg
  | Biop of biop * reg * int * reg
  | Urop of urop * reg * reg
  | Load of reg * reg
  | LoadI of int * reg
  | Store of reg * reg
  | Jump of string
  | CJump of reg * string * string

type code = instruction list SMap.t

val fresh_reg : unit -> reg
val default_temp_regs : reg list
val choose_result_reg : reg option -> reg list -> reg
val available_temp_regs : reg -> reg list -> reg list
val initial_reg_map : string -> string -> var_to_reg

val translate_aexpr :
  a_exp -> reg option -> reg list -> reg_set -> var_to_reg -> reg * instruction list * reg_set

val translate_commutative_aexpr :
  brop ->
  biop ->
  a_exp ->
  a_exp ->
  reg option ->
  reg list ->
	reg_set ->
  var_to_reg ->
  reg * instruction list * reg_set

val translate_minus_aexpr :
  reg option ->
  reg list ->
  a_exp ->
  a_exp ->
	reg_set ->
  var_to_reg ->
  reg * instruction list * reg_set

val translate_bexpr :
  b_exp -> reg option -> reg list -> reg_set -> var_to_reg -> reg * instruction list * reg_set

val translate_stmts : statement list -> var_to_reg -> instruction list * var_to_reg * reg_set
