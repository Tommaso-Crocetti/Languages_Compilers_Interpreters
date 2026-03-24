open Mini_CFG

val visit :
  'node generic_cfg ->
  int list ->
  ('node -> 'set * 'set) ->
  ('node generic_cfg -> int -> 'set * 'set) ->
  ('set * 'set -> 'set * 'set -> bool) ->
  ('node generic_cfg -> 'node -> 'set * 'set -> 'node) ->
  (int -> int list) ->
  (int list -> int -> int list -> int list) ->
  'node generic_cfg
(** Compute the fixpoint of the dataflow analysis by propagating local updates,
    exploiting a worklist-based algorithm *)
