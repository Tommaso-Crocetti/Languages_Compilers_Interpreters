open Mini_CFG
open Mini_RISC
open Mini_RISC_CFG
open Mini_RISC_LiveRegs

(** A vertex in the interference graph, annotated with degree, cost, and color *)
type vertex = { reg : reg; degree : int; cost : float; color : int }

type reg_set = RSet.t

(** Mapping from registers to their costs, according to some heuristic *)
type cost_map = int RMap.t

(** An iterference graph is a mapping from registers to the set of conflicting register *)
type interference_graph = reg_set RMap.t

(** Mapping from spilled registers to memory addresses. *)
type address_map = int RMap.t

(** Mapping from virtual registers to their assigned physical registers. *)
type color_map = reg RMap.t

(** Stack used during graph simplification. *)
module VertexStack : sig
  type t
  val create : unit -> t

  val push : vertex -> t -> unit

  val pop : t -> vertex

  val is_empty : t -> bool
end

type vstack = VertexStack.t

module VSetDegree : Set.S with type elt = vertex

type vset_degree = VSetDegree.t

module VSetCost : Set.S with type elt = vertex

type vset_cost = VSetCost.t

module VSetColor : Set.S with type elt = vertex

type vset_color = VSetColor.t

(** Increment the cost counter associated with a register *)
val increment_cost : reg -> cost_map -> cost_map

(** Compute usage-based costs for all registers in the CFG *)
val compute_cost_map : risc_cfg -> cost_map

(** Extend the variable-to-register map with temporaries discovered in the CFG *)
val extend_reg_map : risc_cfg -> var_to_reg -> var_to_reg

(** Initialize an empty interference graph according to the extended register map *)
val initialize_interference_graph :
  risc_cfg -> var_to_reg -> interference_graph * var_to_reg

(** Add an undirected interference edge between two registers *)
val add_interference_edge :
  interference_graph -> reg -> reg -> interference_graph

(** Build an interference graph that assumes all registers conflict *)
val build_without_liveness :
  interference_graph -> var_to_reg -> interference_graph

(** Build the interference graph using liveness information *)
val build_with_liveness :
  interference_graph -> dataflow_risc_cfg -> interference_graph

(** Rebuild the interference graph and auxiliary sets after a spill *)
val init_interference_graph :
  interference_graph ->
  reg option ->
  cost_map ->
  interference_graph * vset_degree * vset_cost * vset_color

(** Remove a vertex from the auxiliary sets while updating neighbor metadata *)
val implicit_remove_vertex :
  interference_graph ->
  vertex ->
  vset_degree ->
  vset_cost ->
  vset_color ->
  vset_degree * vset_cost * vset_color

(** Simplification step that prefers low-degree vertices *)
val push_by_degree :
  interference_graph ->
  vstack ->
  vset_degree ->
  vset_cost ->
  vset_color ->
  reg_set ->
  int ->
  cost_map ->
  vset_color * reg_set

(** Simplification step that removes the cheapest vertex *)
val push_by_cost :
  interference_graph ->
  vstack ->
  vset_degree ->
  vset_cost ->
  vset_color ->
  reg_set ->
  int ->
  cost_map ->
  vset_color * reg_set

(** Color the simplified graph or trigger a spill *)
val try_color :
  interference_graph ->
  vstack ->
  vset_degree ->
  vset_cost ->
  vset_color ->
  reg_set ->
  int ->
  cost_map ->
  vset_color * reg_set

(** Build the final register coloring map from colored vertices *)
val build_color_map : vset_color -> color_map

(** Apply the color map to every instruction in the CFG *)
val color_cfg : risc_cfg -> color_map -> reg_set -> risc_cfg

(** Compute the memory address associated to a register *)
val spill_slot_of_reg : reg -> int

(** Build the mapping from spilled registers to their memory address *)
val build_address_map : reg_set -> address_map

(** Emit loads for spilled operands prior to an instruction *)
val load_used :
  reg -> address_map -> reg_set -> reg -> instruction list * reg

(** Emit stores for spilled results after an instruction *)
val store_defined :
  address_map -> reg_set -> reg -> instruction list * reg

(** Rewrite a single instruction to insert loads/stores for spilled registers *)
val rewrite_instruction_for_spill :
  address_map -> reg_set -> instruction -> instruction list

(** Rewrite the whole RISC CFG to materialize spilled registers *)
val spill_cfg : risc_cfg -> reg_set -> risc_cfg

(** Insert prologue/epilogue to preserve Rin/Rout *)
val add_prologue_epilogue :
  risc_cfg -> reg_set -> color_map -> risc_cfg

(** Perform the full global register allocation pipeline *)
val global_allocation :
  risc_cfg ->
  dataflow_risc_cfg ->
  int ->
  var_to_reg ->
  bool ->
  risc_cfg * color_map * reg_set
