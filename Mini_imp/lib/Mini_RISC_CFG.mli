open Mini_CFG
open Mini_imp_CFG
open Mini_RISC

exception Error of string

module ISet : Set.S with type elt = int

type risc_cfg = instruction list generic_cfg

val translate_cfg : cfg -> risc_cfg
val string_of_label : int -> string
val append_jump : risc_cfg -> int -> instruction list -> instruction list
val risc_cfg_to_code : (instruction -> string) -> risc_cfg -> string
