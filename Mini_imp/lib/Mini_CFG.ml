open Mini_imp_AST

module SSet = Mini_Modules.SSet

type var_set = SSet.t

module NMap = Mini_Modules.NMap

type out_node =
  | Single of int
  | Pair of int * int

type 'a generic_cfg = 
{
  nodes: 'a NMap.t;
  edges: out_node NMap.t;
  initial: int;
  final: int list;
  input_var: string;
  output_var: string;
  all_vars: var_set;
}

let empty_generic_cfg: 'a generic_cfg =
  {
    nodes = NMap.empty;
    edges = NMap.empty;
    initial = 0;
    final = [0];
    input_var = "";
    output_var = "";
    all_vars = SSet.empty;
  }

let generic_add_node
  (cfg : 'a generic_cfg)
  (node_id : int)
  (node : 'a)
  : 'a generic_cfg =
  { cfg with nodes = NMap.add node_id node cfg.nodes }

let generic_add_edge
  (cfg : 'a generic_cfg)
  (src : int)
  (dst : out_node)
  : 'a generic_cfg =
  { cfg with edges = NMap.add src dst cfg.edges }

let rec find_all_vars_aexp (a: a_exp) : var_set =
  match a with
  | Aval _ -> SSet.empty
  | Var x -> SSet.singleton x
  | Of_Bool b -> find_all_vars_bexp b
  | Plus (a1, a2)
  | Minus (a1, a2)
  | Times (a1, a2) ->
      SSet.union (find_all_vars_aexp a1) (find_all_vars_aexp a2)

and find_all_vars_bexp (b: b_exp) : var_set =
  match b with
  | Bval _ -> SSet.empty
  | And (b1, b2)
  | Or (b1, b2) ->
      SSet.union (find_all_vars_bexp b1) (find_all_vars_bexp b2)
  | Not b1 -> find_all_vars_bexp b1
  | Minor (a1, a2) ->
      SSet.union (find_all_vars_aexp a1) (find_all_vars_aexp a2)

let rec find_all_vars_command (c: command) : var_set =
  match c with
  | Skip -> SSet.empty
  | Assign (x, a) -> SSet.add x (find_all_vars_aexp a)
  | Seq (c1, c2) ->
      SSet.union (find_all_vars_command c1) (find_all_vars_command c2)
  | If (b, c1, c2) ->
      SSet.union
        (find_all_vars_bexp b)
        (SSet.union (find_all_vars_command c1) (find_all_vars_command c2))
  | While (b, c1) ->
      SSet.union (find_all_vars_bexp b) (find_all_vars_command c1)
