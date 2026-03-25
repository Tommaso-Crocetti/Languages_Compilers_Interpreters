open Mini_CFG
open Mini_imp_CFG
open Mini_RISC

exception Error of string

(** A RISC CFG is a generic CFG where nodes are associated to lists of RISC instructions *)
type risc_cfg = instruction list generic_cfg

(** Turns a CFG to a RISC CFG by translating the list of maximal statements of the original CFG *)
val build_risc_cfg : cfg -> risc_cfg * var_to_reg * reg
(** Generates the RISC label for a node Id *)
val string_of_label : int -> string
(** Appends a jump instruction representing the outgoing edge of the node:
  - Single outgoing edge: append a unconditional jump
  - Pair of outgoing edges: append a conditional jump based on the guard register
 *)
val append_jump :
  reg -> risc_cfg -> int -> instruction list -> instruction list
(** Given a RISC CFG, appends jump instructions at the end of the block, 
  representing the node outgoing edges *)
val risc_cfg_with_jumps :
  reg -> risc_cfg -> risc_cfg
(** Converts a RISC CFG into a RISC valid program *)
val risc_cfg_to_code :
   risc_cfg -> (instruction -> string) -> string
