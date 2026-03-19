open Mini_CFG
open Mini_RISC
open Mini_RISC_CFG

type dataflow_risc_node =
  {
    instructions : instruction list;
    in_regs : reg_set;
    out_regs : reg_set;
  }

type dataflow_risc_cfg = dataflow_risc_node generic_cfg

val build_dataflow_risc_cfg : risc_cfg -> dataflow_risc_cfg
val liveness_local_update : dataflow_risc_cfg -> int -> reg_set * reg_set -> reg_set * reg_set
val liveness_global_update : dataflow_risc_cfg -> dataflow_risc_cfg
