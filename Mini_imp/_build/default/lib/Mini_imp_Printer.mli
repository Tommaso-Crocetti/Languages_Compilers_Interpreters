open Mini_imp_Parser
open Mini_imp_AST
open Mini_CFG
open Mini_imp_CFG
open Mini_RISC
open Mini_RISC_CFG

val string_of_token : token -> string
val string_of_position : Lexing.position -> string
val aexp_to_string : a_exp -> string
val bexp_to_string : b_exp -> string
val command_to_string : command -> string
val program_to_string : program -> string
val string_of_cfg_statement : statement -> string
val string_of_cfg_out_node : out_node -> string
val generic_cfg_to_string : (int -> 'a -> string) -> 'a generic_cfg -> string
val cfg_to_string : cfg -> string
val string_of_risc_reg : reg -> string
val string_of_risc_brop : brop -> string
val string_of_risc_biop : biop -> string
val string_of_risc_urop : urop -> string
val string_of_risc_instruction : instruction -> string
val risc_cfg_to_string : risc_cfg -> string
val reg_map_to_string : var_to_reg -> string
val risc_cfg_and_reg_map_to_string : risc_cfg -> var_to_reg -> string
val color_map_to_string : reg RMap.t -> string
val spilled_registers_to_string : reg_set -> string
