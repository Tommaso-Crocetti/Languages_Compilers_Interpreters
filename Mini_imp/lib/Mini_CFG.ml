open Mini_imp_AST
module SSet = Mini_Modules.SSet

type var_set = Mini_imp_AST.var_set

module IMap = Mini_Modules.IMap

type out_node = Single of int | Pair of int * int

type 'a generic_cfg = {
  nodes : 'a IMap.t;
  edges : out_node IMap.t;
  initial : int;
  final : int list;
  input_var : string;
  output_var : string;
  all_vars : var_set;
}

let empty_cfg : 'a generic_cfg =
  {
    nodes = IMap.empty;
    edges = IMap.empty;
    initial = 0;
    final = [ 0 ];
    input_var = "";
    output_var = "";
    all_vars = SSet.empty;
  }

let add_node (cfg : 'a generic_cfg) (node_id : int) (node : 'a) : 'a generic_cfg
    =
  { cfg with nodes = IMap.add node_id node cfg.nodes }

let add_edge (cfg : 'a generic_cfg) (src : int) (dst : out_node) :
    'a generic_cfg =
  { cfg with edges = IMap.add src dst cfg.edges }

let find_predecessors (g : 'a generic_cfg) (node_id : int) : int list =
  List.filter_map
    (fun (src_id, out_nodes) ->
      match out_nodes with
      | Single dst_id when dst_id = node_id -> Some src_id
      | Pair (dst_id1, dst_id2) when dst_id1 = node_id || dst_id2 = node_id ->
          Some src_id
      | _ -> None)
    (IMap.bindings g.edges)
