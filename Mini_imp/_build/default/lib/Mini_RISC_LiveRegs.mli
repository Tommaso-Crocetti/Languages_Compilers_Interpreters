open Mini_CFG
open Mini_RISC
open Mini_RISC_CFG

type dataflow_risc_node = {
  instructions : instruction list;
  in_regs : reg_set;
  out_regs : reg_set;
}
(** A node in the dataflow graph contains:
    - instructions: the list of RISC instructions in the node
    - in_regs: the set of registers live at the entry of this instruction
    - out_regs: the set of registers live at the exit of this instruction *)

type dataflow_risc_cfg = dataflow_risc_node generic_cfg
(** A dataflow RISC CFG is a generic CFG where nodes are associated to dataflow
    RISC nodes *)

val build_dataflow_risc_cfg : risc_cfg -> reg -> dataflow_risc_cfg
(** Builds the initial dataflow RISC CFG from a RISC control flow graph, by
    assigning the bottom of the CPO (the empty set of registers) to each in_regs
    and out_regs *)

val liveness_local_update :
  dataflow_risc_cfg -> int -> reg_set * reg_set -> reg_set * reg_set
(** Performs a local update, redefining the defined in_regs and out_regs of a
    node based on its successors and previously live registers *)

val liveness_global_update : dataflow_risc_cfg -> dataflow_risc_cfg
(** Computes the global dataflow analysis exploiting the worklist-based
    algorithm that applies the local update until a fixpoint is reached *)

val print_in_out_regs : dataflow_risc_cfg -> unit
