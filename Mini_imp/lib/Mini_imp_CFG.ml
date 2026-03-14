type statement =
  | Skip
  | Assign of Mini_imp.var * Mini_imp.a_exp
  | Guard of Mini_imp.b_exp

type out_node = 
  | Single of int
  | Pair of int * int

module NMap = Map.Make(struct type t = int let compare = compare end)

module EdgeOrd : Set.OrderedType with type t = int = struct
  type t = int
  let compare = compare
end

module EMap = Map.Make(EdgeOrd)

type cfg = 
{
  nodes: statement list NMap.t;
  edges: out_node EMap.t;
  initial: int;
  final: int list;
}

let fresh_id =
  let next = ref (-1) in
  fun () ->
    incr next;
    !next

let empty_cfg : cfg =
  {
    nodes = NMap.empty;
    edges = EMap.empty;
    initial = 0;
    final = [0];
  }

let get_node (g: cfg) (id: int) : statement list =
  match NMap.find_opt id g.nodes with
  | Some stmts -> stmts
  | None -> failwith ("Node with id " ^ string_of_int id ^ " not found in CFG.")

let print_final_node (cfg: cfg) (id: int) : unit =
  print_endline ("Inserted node: " ^ string_of_int id);
  match cfg.final with
  | [n] -> print_endline ("Final node: " ^ string_of_int n);
  | [id1; id2] -> print_endline ("Final pair: (" ^ string_of_int id1 ^ ", " ^ string_of_int id2 ^ ")")
  | [] -> print_endline "Final node: none"
  | _ -> print_endline "Final node: multiple nodes"

let add_node (g: cfg) (stmts: statement list) : cfg =
  let id = fresh_id () in
  let cfg = { g with nodes = NMap.add id stmts g.nodes; final = [id] } in
  cfg

let add_edge (g: cfg) (src: int) (dst: out_node) : cfg =
  { g with edges = EMap.add src dst g.edges }

let make_cfg (p: Mini_imp.program) : cfg =
    let rec dfs (c: Mini_imp.command) (g: cfg) (stmts: statement list) (last_command: Mini_imp_Parser.token) : cfg * statement list * Mini_imp_Parser.token =
      match c with
      | Mini_imp.Skip -> (g, stmts, Mini_imp_Parser.SKIP)
      | Mini_imp.Assign (x, a) -> 
        (match stmts with
        | [Skip] -> (g, [Assign (x, a)], Mini_imp_Parser.ASSIGN)
        | _ -> (g, Assign (x, a)::stmts, Mini_imp_Parser.ASSIGN))
      | Mini_imp.Seq (c1, c2) -> 
        let (g, stmts1, prev_command) = dfs c1 g stmts last_command in
        let (g, stmts2, prev_command) = dfs c2 g stmts1 prev_command in
        (g, stmts2, prev_command)
      | Mini_imp.If (b, c1, c2) ->
        let previous_final_nodes = g.final in 
        let g' = add_node g (List.rev (Guard b::stmts)) in
        let new_final_node = match g'.final with
          | [n] -> n
          | _ -> failwith "Unexpected: after adding a node, the final should be a single node."
        in
        (* Backpatch the previous final node to point to the new node *)
        let g' = if last_command <> Mini_imp_Parser.ELSE then
              List.fold_left (fun cfg node_id -> add_edge cfg node_id (Single new_final_node)) g' previous_final_nodes
        else g' in
        let (g1, then_stmts, previous_command) = dfs c1 g' [] Mini_imp_Parser.THEN in
        let then_previous_final_nodes = g1.final in
        let g1' = (
              if then_stmts != [] then
                let g1'' = add_node g1 (List.rev then_stmts) in
                let then_join_node = match g1''.final with
                  | [n] -> n
                  | _ -> failwith "Unexpected: after processing the then branch, the final should be a single node."
                in
                List.fold_left
                  (fun acc node_id ->
                    if node_id = then_join_node || EMap.mem node_id acc.edges then acc
                    else add_edge acc node_id (Single then_join_node))
                  g1''
                  then_previous_final_nodes
              else g1
          ) in 
        let then_final_node = match g1'.final with
          | [n] -> n
          | _ -> failwith "Unexpected: after processing the then branch, the final should be a single node."
        in
        let (g2, else_stmts, previous_command) = dfs c2 g1' [] Mini_imp_Parser.ELSE in
        let else_previous_final_nodes = g2.final in
        let g2' = (
            if else_stmts != [] then
              let g2'' = add_node g2 (List.rev else_stmts) in
              let else_final_node = match g2''.final with
                | [n] -> n
                | _ -> failwith "Unexpected: after processing the else branch, the final should be a single node."
              in
              List.fold_left
                (fun acc node_id ->
                  if node_id = then_final_node || node_id = else_final_node || EMap.mem node_id acc.edges then acc
                  else add_edge acc node_id (Single else_final_node))
                g2''
                else_previous_final_nodes
            else g2
        ) in
        let else_final_node = match g2'.final with
          | [n] -> n
          | _ -> failwith "Unexpected: after processing the else branch, the final should be a single node."
        in
        let g3 = add_edge g2' new_final_node (Pair (new_final_node + 1, then_final_node + 1)) in
        let g_f = {g3 with final = [then_final_node; else_final_node]} in
        (g_f, [Skip], previous_command)
      | Mini_imp.While (b, c) ->
        let previous_final_nodes = g.final in 
        let pre_guard_stmts =
          if stmts = [] then [Skip]
          else List.rev stmts
        in
        let g' = add_node (add_node g pre_guard_stmts) [Guard b] in
        (* Backpatch the previous final node to point to the new node *)
        let new_final_node = match g'.final with
          | [n] -> n
          | _ -> failwith "Unexpected: after adding a node, the final should be a single node."
        in
        let pre_guard_node = new_final_node - 1 in
        let g' = if last_command <> Mini_imp_Parser.ELSE then
              List.fold_left (fun cfg node_id -> add_edge cfg node_id (Single pre_guard_node)) g' previous_final_nodes
            else g'
        in
        let g_guard = add_edge g' pre_guard_node (Single new_final_node) in
        let (g_while, body_stmts, previous_command) = dfs c g_guard [] Mini_imp_Parser.WHILE in
        let pending_body_final_nodes = g_while.final in
        let g_while = (
            if body_stmts != [] then
              let g_body = add_node g_while (List.rev body_stmts) in
              let body_join_node = match g_body.final with
                | [n] -> n
                | _ -> failwith "Unexpected: after processing the body, the final should be a single node."
              in
              List.fold_left
                (fun acc node_id ->
                  if node_id = body_join_node || EMap.mem node_id acc.edges then acc
                  else add_edge acc node_id (Single body_join_node))
                g_body
                pending_body_final_nodes
            else g_while
        ) in
        let body_final_node = match g_while.final with
          | [n] -> n
          | _ -> failwith "Unexpected: after processing the body, the final should be a single node."
        in
        let g1 = add_edge (add_edge g_while new_final_node (Pair (new_final_node+1, body_final_node+1))) body_final_node (Single new_final_node) in
        let g_f = {g1 with final = [new_final_node]} in
        (g_f, [Skip], previous_command)
    in let cfg, final_stmts, final_command = dfs p.Mini_imp.body empty_cfg [] Mini_imp_Parser.SKIP in
    let pending_final_nodes = cfg.final in
    let cfg' = if final_stmts != [] then add_node cfg (List.rev final_stmts) else cfg in
    let final_node = match cfg'.final with
      | [n] -> n
      | _ -> failwith "Unexpected: after processing the entire program, the final should be a single node."
    in
    if final_stmts != [] then
      List.fold_left
        (fun acc node_id ->
          if node_id = final_node || EMap.mem node_id acc.edges then acc
          else add_edge acc node_id (Single final_node))
        cfg'
        pending_final_nodes
    else cfg'

let cfg_to_string (g: cfg) : string =
    let node_str (id: int) (stmts: statement list) : string =
        let stmts_str = List.map (function
        | Skip -> "Skip"
        | Assign (x, a) -> "Assign(" ^ x ^ ", " ^ Mini_imp.aexp_to_string a ^ ")"
        | Guard b -> "Guard(" ^ Mini_imp.bexp_to_string b ^ ")"
        ) stmts in
        string_of_int id ^ ": " ^ String.concat "; " stmts_str
    in
    let edge_str (src: int) (dst: out_node) : string =
        let dst_str = match dst with
        | Single n -> string_of_int n
        | Pair (id1, id2) -> "(" ^ string_of_int id1 ^ ", " ^ string_of_int id2 ^ ")"
        in
        string_of_int src ^ " -> " ^ dst_str
    in
    let nodes_str = NMap.bindings g.nodes |> List.map (fun (id, stmts) -> node_str id stmts) |> String.concat "\n" in
    let edges_str = EMap.bindings g.edges |> List.map (fun (src, dst) -> edge_str src dst) |> String.concat "\n" in
    "Nodes:\n" ^ nodes_str ^ "\nEdges:\n" ^ edges_str