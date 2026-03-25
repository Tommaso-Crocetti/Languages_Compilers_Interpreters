open Mini_imp_AST
open Mini_imp_CFG

exception Error of string

type reg = Rin | Rout | Ra | Rb | RVar of int

module SMap = Mini_Modules.SMap

(** Maps variable names to registers *)
type var_to_reg = reg SMap.t

module RSet : Set.S with type elt = reg
module RMap : Map.S with type key = reg

type reg_set = RSet.t

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

(* A RISC code is a mapping from labels to lists of instructions *)
type code = instruction list SMap.t

(** Returns a fresh register *)
val fresh_reg : unit -> reg
(** Returns the current checkpoint for the register creator *)
val reg_allocator_checkpoint : unit -> int
(** Resets the register allocator to a specific checkpoint *)
val reset_reg_allocator : int -> unit
(* Choose the result register checking if a target is available, 
  or if a temporary register is avaiable. Otherwise, it creates a new one *)
val choose_result_reg : reg option -> reg list -> reg
(** Removes a reserved register from the list of temporary registers *)
val available_temp_regs : reg -> reg list -> reg list
(** Initializes the register map, associatig "in" and "out" to the input and output variables *)
val initial_reg_map : string -> string -> var_to_reg

(** Translate a generic arithmetic expression into a RISC instruction list *)
val translate_aexpr :
  a_exp -> reg option -> reg list -> var_to_reg -> reg * instruction list

(** Translates a commutative binary operation into RISC instructions, exploiting the commutativity *)  
val translate_commutative_aexpr :
  brop ->
  biop ->
  a_exp ->
  a_exp ->
  reg option ->
  reg list ->
  var_to_reg ->
  reg * instruction list

(** Translate a subtraction of generic arithmetic expressions *)
val translate_minus_aexpr :
  reg option ->
  reg list ->
  a_exp ->
  a_exp ->
  var_to_reg ->
  reg * instruction list

(** Translate a generic boolean expression into a RISC instruction list *)
val translate_bexpr :
  b_exp -> reg option -> reg list -> var_to_reg -> reg * instruction list

(** Translate a list of statements into a RISC instruction list *)
val translate_stmts :
  statement list ->
  reg list ->
  reg ->
  var_to_reg ->
  instruction list * var_to_reg

(** Find the register defined by an instruction, if any *)
val find_defined_reg : instruction -> reg option
(** Find the registers used by an instruction *)
val find_used_regs : instruction -> reg_set
(** Find the registers used and defined by a list of instructions *)
val find_used_defined_regs : instruction list -> reg_set * reg_set
