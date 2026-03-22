open Mini_CFG
open Mini_imp_CFG
open Mini_RISC

exception Error of string

type risc_cfg = instruction list generic_cfg

val build_risc_cfg : cfg -> risc_cfg * var_to_reg * reg
val string_of_label : int -> string
val risc_cfg_with_jumps : reg -> risc_cfg -> risc_cfg
val risc_cfg_to_code : (instruction -> string) -> reg -> risc_cfg -> string
