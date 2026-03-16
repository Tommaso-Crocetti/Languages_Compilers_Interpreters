type statement =
  | Skip
  | Assign of Mini_imp_Interpreter.var * Mini_imp_Interpreter.a_exp
  | Guard of Mini_imp_Interpreter.b_exp

type out_node =
  | Single of int
  | Pair of int * int

module NMap = Map.Make(struct type t = int let compare = compare end)

type cfg = 
{
  nodes: statement list NMap.t;
  edges: out_node NMap.t;
  initial: int;
  final: int list;
}

(** Returns fresh node Id *)
let fresh_id =
  let next = ref (-1) in
  fun () ->
    incr next;
    !next

let empty_cfg : cfg =
  {
    nodes = NMap.empty;
    edges = NMap.empty;
    initial = 0;
    final = [0];
  }

(** Adds a new node to the CFG with the given statements, always ensuring that it is the final node *)
let add_node (g: cfg) (stmts: statement list) : cfg =
  let id = fresh_id () in
  let cfg = { g with nodes = NMap.add id stmts g.nodes; final = [id] } in
  cfg

(* Adds an edge to the CFG *)
let add_edge (g: cfg) (src: int) (dst: out_node) : cfg =
  { g with edges = NMap.add src dst g.edges }

(* Helper functions that retrives the next node in the cfg *)
let next_node_id (g: cfg) : int =
  match List.rev (NMap.bindings g.nodes) with
  | (node_id, _) :: _ -> node_id + 1
  | [] -> 0

(* Helper backpatch function *)
let connect_pending_node (g: cfg) (src: int) (dst: int) : cfg =
  if src = dst then g
  else
    match NMap.find_opt src g.edges with
    (* If no edge exists from the source node, create a new edge *)
    | None -> add_edge g src (Single dst)
    (* If an edge already exists, there is no need to update the cfg *)
    | Some (Single _) -> g
    (* If an edge already exists and is a pair, we need to update only the right side *)
    | Some (Pair (left_dst, _)) -> add_edge g src (Pair (left_dst, dst))


(* Given a .mimp program, incrementally builds a control flow graph *)
let make_cfg (p: Mini_imp_Interpreter.program) : cfg =
    (* Recursive function to process a command and incrementally update the CFG,
    according also to the pending statements and the last seen command *)
    let rec dfs (c: Mini_imp_Interpreter.command) (g: cfg) (stmts: statement list) (last_command: Mini_imp_Parser.token) : cfg * statement list * Mini_imp_Parser.token =
      match c with
      (* Process a skip command, just inform that Skip was the last command *)
      | Mini_imp_Interpreter.Skip -> (g, stmts, Mini_imp_Parser.SKIP)
      (* Process an assignment command, add it to the pending statements *)
      | Mini_imp_Interpreter.Assign (x, a) -> 
        (match stmts with
        | [Skip] -> (g, [Assign (x, a)], Mini_imp_Parser.ASSIGN)
        | _ -> (g, Assign (x, a)::stmts, Mini_imp_Parser.ASSIGN))
      (* Process a sequence of commands, in the given order *)
      | Mini_imp_Interpreter.Seq (c1, c2) -> 
        let (g, stmts1, prev_command) = dfs c1 g stmts last_command in
        let (g, stmts2, prev_command) = dfs c2 g stmts1 prev_command in
        (g, stmts2, prev_command)
      (* Process an if statement, creating a new node with pending statements + guard, 
      then recursively processing the branches *)
      | Mini_imp_Interpreter.If (b, c1, c2) ->
        (* Retrieve the previous final nodes *)
        let previous_final_nodes = g.final in 
        let pending_stmts =
          match stmts with
          | [Skip] -> []
          | _ -> stmts
        in
        (* Add the guard statement to the pending statements, creating a new node *)
        let g' = add_node g (List.rev (Guard b::pending_stmts)) in
        let new_final_node = match g'.final with
          | [n] -> n
          | _ -> failwith "Unexpected: after adding a node, the final should be a single node."
        in
        (* Avoid backpatching if the last command is ELSE *)
        let g' = if last_command <> Mini_imp_Parser.ELSE then
              List.fold_left (fun cfg node_id -> add_edge cfg node_id (Single new_final_node)) g' previous_final_nodes
        else g' in
        (* Compute the entry node for the then branch *)
        let then_entry_node = next_node_id g' in
        (* Process the then branch *)
        let (g1, then_stmts, previous_command) = dfs c1 g' [] Mini_imp_Parser.THEN in
        (* Handle of the then pending statements *)
        let then_stmts =
          match then_stmts with
          | [Skip] -> []
          | _ -> then_stmts
        in
        (* Retrieve the previous final nodes of the then branch *)
        let then_previous_final_nodes = g1.final in
        let g1' =
          if then_stmts <> [] then
            (* Create a new node for the then pending statements *)
            let g1'' = add_node g1 (List.rev then_stmts) in
            let then_join_node = match g1''.final with
              | [n] -> n
              | _ -> failwith "Unexpected: after processing the then branch, the final should be a single node."
            in
            (* Backpatch: the previous final nodes of the then branch 
            must point to the new node *)
            List.fold_left
              (fun acc node_id ->
                connect_pending_node acc node_id then_join_node)
              g1''
              then_previous_final_nodes
          (* Handle the case where there are no statements in the then branch *)
          else if then_previous_final_nodes = [new_final_node] then
            add_node g1 [Skip]
          (* If the then branch is not empty and has no pending statements, it is correct *)
          else g1
        in
        (* Retrieve the final nodes of the then branch *)
        let then_final_nodes = g1'.final in
        (* Retrieve the entry node for the else branch *)
        let else_entry_node = next_node_id g1' in
        (* Process the else branch *)
        let (g2, else_stmts, previous_command) = dfs c2 g1' [] Mini_imp_Parser.ELSE in
        (* Handle the else pending statements *)
        let else_stmts =
          match else_stmts with
          | [Skip] -> []
          | _ -> else_stmts
        in
        (* Retrieve the previous final nodes of the else branch *)
        let else_previous_final_nodes = g2.final in
        let g2' =
          if else_stmts <> [] then
            (* Create a new node for the else pending statements *)
            let g2'' = add_node g2 (List.rev else_stmts) in
            let else_final_node = match g2''.final with
              | [n] -> n
              | _ -> failwith "Unexpected: after processing the else branch, the final should be a single node."
            in
            (* Backpatch: the previous final nodes of the else branch
            must point to the new node,
            note that backpatch is skipped if the node is in the then branch *)
            List.fold_left
              (fun acc node_id ->
                if List.mem node_id then_final_nodes then acc
                else connect_pending_node acc node_id else_final_node)
              g2''
              else_previous_final_nodes
          (* Handle the case where there are no statements in the else branch *)
          else if else_previous_final_nodes = then_final_nodes then
            add_node g2 [Skip]
          (* If the else branch is not empty and has no pending statements, it is correct *)
          else g2
        in
        let else_final_nodes = g2'.final in
        (* Connect the guard node to the entry nodes of the then and else branches *)
        let g3 = add_edge g2' new_final_node (Pair (then_entry_node, else_entry_node)) in
        (* The final resulting node is the concatenation of all the branches final nodes *)
        let g_f = {g3 with final = then_final_nodes @ else_final_nodes} in
        (g_f, [Skip], previous_command)
      (* Process a while statement, creating a new node with pending statements,
      a specfic node for the guard, then recursively processing the body *)
      | Mini_imp_Interpreter.While (b, c) ->
        (* Retrieve the previous final nodes *)
        let previous_final_nodes = g.final in 
        (* If there are no pending statements, it is necessary to create a skip statement *)
        let pre_guard_stmts =
          if stmts = [] then [Skip]
          else List.rev stmts
        in
        (* Create a new node for the pre-guard statements *)
        let g_pre = add_node g pre_guard_stmts in
        let pre_guard_node = match g_pre.final with
          | [n] -> n
          | _ -> failwith "Unexpected: after adding the pre-guard node, the final should be a single node."
        in
        (* Avoid backpatching if the last command is ELSE *)
        let g' = if last_command <> Mini_imp_Parser.ELSE then
              List.fold_left (fun cfg node_id -> add_edge cfg node_id (Single pre_guard_node)) g_pre previous_final_nodes
            else g_pre
        in
        (* Add the guard statement node *)
        let g' = add_node g' [Guard b] in
        let guard_node = match g'.final with
          | [n] -> n
          | _ -> failwith "Unexpected: after adding a node, the final should be a single node."
        in
        (* Connect the pre-guard node to the guard node *)
        let g_guard = add_edge g' pre_guard_node (Single guard_node) in
        (* Process the body of the while loop *)
        let body_entry_node = next_node_id g_guard in
        let (g_while, body_stmts, previous_command) = dfs c g_guard [] Mini_imp_Parser.WHILE in
        let body_final_nodes = g_while.final in
        let g_while =
          (* Handle the body pending statements *)
          if body_stmts <> [] then
            let g_body = add_node g_while (List.rev body_stmts) in
            let body_join_node = match g_body.final with
              | [n] -> n
              | _ -> failwith "Unexpected: after processing the body, the final should be a single node."
            in
            (* Backpatch: the previous final nodes of the body must point to the new node *)
            List.fold_left
              (fun acc node_id ->
                connect_pending_node acc node_id body_join_node)
              g_body
              body_final_nodes
          (* Handle the case where there are no statements in the body *)
          else if body_final_nodes = [guard_node] then
            add_node g_while [Skip]
          (* If the body is not empty and has no pending statements, it is correct *)
          else g_while
        in
        (* Connect the guard node to the body initial node and (temporarily) to itself,
        as the following node is still not defined *)
        let g1 = add_edge g_while guard_node (Pair (body_entry_node, guard_node)) in
        (* Connect the body final nodes back to the guard node *)
        let g2 =
          List.fold_left
            (fun acc node_id -> connect_pending_node acc node_id guard_node)
            g1
            g_while.final
        in
        (* Set the final node to the guard node, 
        as the current final node is the last node of the body +
        future backpatch will adjust the guard node self-loop *)
        let g_f = {g2 with final = [guard_node]} in
        (g_f, [Skip], previous_command)
    (* Start the cfg construction *)
    in let cfg, final_stmts, final_command = dfs p.Mini_imp_Interpreter.body empty_cfg [] Mini_imp_Parser.SKIP in
    (* Final backpatch with the last list of statements of the program *)
    let pending_final_nodes = cfg.final in
    (* Create a node with the final pending statements *)
    let cfg' = if final_stmts != [] then add_node cfg (List.rev final_stmts) else cfg in
    let final_node = match cfg'.final with
      | [n] -> n
      | _ -> failwith "Unexpected: after processing the entire program, the final should be a single node."
    in
    (* Backpatch: the previous final nodes must point to the new final node *)
    if final_stmts != [] then
      List.fold_left
        (fun acc node_id ->
          connect_pending_node acc node_id final_node)
        cfg'
        pending_final_nodes
    else cfg'