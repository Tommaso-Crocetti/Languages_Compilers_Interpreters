open Mini_imp_Parser
open Mini_imp_AST
open Mini_CFG

exception Error of string

type statement =
  | Skip
  | Assign of string * a_exp
  | Guard of b_exp

type cfg = (statement list * var_set) generic_cfg

(* Returns fresh node Id *)
let fresh_id =
  let next = ref (-1) in
  fun () ->
    incr next;
    !next

(* Helper functions that retrives the next node in the cfg *)
let next_node_id (g: cfg) : int =
  match List.rev (IMap.bindings g.nodes) with
  | (node_id, _) :: _ -> node_id + 1
  | [] -> 0

let add_block (g: cfg) (stmts: statement list) (def_vars: var_set) : cfg * int =
  let node_id = next_node_id g in
  let g = add_node g node_id (stmts, def_vars) in
  ({ g with final = [node_id] }, node_id)

(* Helper backpatch function *)
let connect_pending_node (g: cfg) (src: int) (dst: int) : cfg =
  if src = dst then g
  else
    match IMap.find_opt src g.edges with
    (* If no edge exists from the source node, create a new edge *)
    | None -> add_edge g src (Single dst)
    (* If an edge already exists, there is no need to update the cfg *)
    | Some (Single _) -> g
    (* If an edge already exists and is a pair, we need to update only the right side *)
    | Some (Pair (left_dst, _)) -> add_edge g src (Pair (left_dst, dst))

(* Given a .mimp program, incrementally builds a control flow graph *)
let build_cfg (p: program) : cfg =
  let all_vars = find_all_vars_command p.body in
    let initial_cfg = {empty_cfg with input_var = p.input_var; output_var = p.output_var; all_vars = all_vars} in
    (* Recursive function to process a command and incrementally update the CFG,
    according also to the pending statements and the last seen command *)
    let rec visit (c: command) (g: cfg) (stmts: statement list) (def_vars: var_set) (last_command: token) : cfg * statement list * var_set * token =
      match c with
      (* Process a skip command, just inform that Skip was the last command *)
      | Skip -> (g, stmts, def_vars, SKIP)
      (* Process an assignment command, add it to the pending statements *)
      | Assign (x, a) ->
        (match stmts with
        | [Skip] -> (g, [Assign (x, a)], SSet.add x def_vars, ASSIGN)
        | _ -> (g, Assign (x, a)::stmts, SSet.add x def_vars, ASSIGN))
      (* Process a sequence of commands, in the given order *)
      | Seq (c1, c2) -> 
        let (g, stmts1, def_vars1, prev_command) = visit c1 g stmts def_vars last_command in
        let (g, stmts2, def_vars2, prev_command) = visit c2 g stmts1 def_vars1 prev_command in
        (g, stmts2, def_vars2, prev_command)
      (* Process an if statement, creating a new node with pending statements + guard, 
      then recursively processing the branches *)
      | If (b, c1, c2) ->
        (* Retrieve the previous final nodes *)
        let previous_final_nodes = g.final in 
        let pending_stmts =
          match stmts with
          | [Skip] -> []
          | _ -> stmts
        in
        (* Add the guard statement to the pending statements, creating a new node *)
        let g', new_final_node = add_block g (List.rev (Guard b::pending_stmts)) def_vars in
        (* Avoid backpatching if the last command is ELSE *)
        let g' = if last_command <> ELSE then
              List.fold_left (fun cfg node_id -> connect_pending_node cfg node_id new_final_node) g' previous_final_nodes
        else g' in
        (* Compute the entry node for the then branch *)
        let then_entry_node = next_node_id g' in
        (* Process the then branch *)
        let (g1, then_stmts, def_vars_then, previous_command) = visit c1 g' [] SSet.empty last_command in
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
            let g1'', then_join_node = add_block g1 (List.rev then_stmts) def_vars_then in
            (* Backpatch: the previous final nodes of the then branch 
            must point to the new node *)
            List.fold_left
              (fun acc node_id ->
                connect_pending_node acc node_id then_join_node)
              g1''
              then_previous_final_nodes
          (* Handle the case where there are no statements in the then branch *)
          else if then_previous_final_nodes = [new_final_node] then
            fst (add_block g1 [Skip] SSet.empty)
          (* If the then branch is not empty and has no pending statements, it is correct *)
          else g1
        in
        (* Retrieve the final nodes of the then branch *)
        let then_final_nodes = g1'.final in
        (* Retrieve the entry node for the else branch *)
        let else_entry_node = next_node_id g1' in
        (* Process the else branch *)
        let (g2, else_stmts, def_vars_else, previous_command) = visit c2 {g1' with final = []} [] SSet.empty last_command in
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
            let g2'', else_final_node = add_block g2 (List.rev else_stmts) def_vars_else in
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
            fst (add_block g2 [Skip] SSet.empty)
          (* If the else branch is not empty and has no pending statements, it is correct *)
          else g2
        in
        let else_final_nodes = g2'.final in
        (* Connect the guard node to the entry nodes of the then and else branches *)
        let g3 = add_edge g2' new_final_node (Pair (then_entry_node, else_entry_node)) in
        (* The final resulting node is the concatenation of all the branches final nodes *)
        let g_f = {g3 with final = then_final_nodes @ else_final_nodes} in
        (g_f, [Skip], SSet.empty, previous_command)
      (* Process a while statement, creating a new node with pending statements,
      a specfic node for the guard, then recursively processing the body *)
      | While (b, c) ->
        (* Retrieve the previous final nodes *)
        let previous_final_nodes = g.final in 
        (* If there are no pending statements, it is necessary to create a skip statement *)
        let pre_guard_stmts =
          if stmts = [] then [Skip]
          else List.rev stmts
        in
        (* Create a new node for the pre-guard statements *)
        let g_pre, pre_guard_node = add_block g pre_guard_stmts def_vars in
        (* Avoid backpatching if the last command is ELSE *)
        let g' = if last_command <> ELSE then
              List.fold_left (fun cfg node_id -> connect_pending_node cfg node_id pre_guard_node) g_pre previous_final_nodes
            else g_pre
        in
        (* Add the guard statement node *)
        let g', guard_node = add_block g' [Guard b] SSet.empty in
        (* Connect the pre-guard node to the guard node *)
        let g_guard = add_edge g' pre_guard_node (Single guard_node) in
        (* Process the body of the while loop *)
        let body_entry_node = next_node_id g_guard in
        let (g_while, body_stmts, def_vars_body, previous_command) = visit c g_guard [] SSet.empty last_command in
        let body_final_nodes = g_while.final in
        let g_while =
          (* Handle the body pending statements *)
          if body_stmts <> [Skip] then
            let g_body, body_join_node = add_block g_while (List.rev body_stmts) def_vars_body in
            (* Backpatch: the previous final nodes of the body must point to the new node *)
            List.fold_left
              (fun acc node_id ->
                connect_pending_node acc node_id body_join_node)
              g_body
              body_final_nodes
          (* Handle the case where there are no statements in the body *)
          else if body_final_nodes = [guard_node] then
            fst (add_block g_while [Skip] SSet.empty)
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
        (g_f, [Skip], SSet.empty, previous_command)
    (* Start the cfg construction *)
    in let cfg, final_stmts, final_def_vars, final_command = visit p.body initial_cfg [] SSet.empty SKIP in
    (* Final backpatch with the last list of statements of the program *)
    let pending_final_nodes = cfg.final in
    (* Create a node with the final pending statements *)
    if final_stmts <> [] then
      let cfg', final_node = add_block cfg (List.rev final_stmts) final_def_vars in
      List.fold_left
        (fun acc node_id -> connect_pending_node acc node_id final_node)
        cfg'
        pending_final_nodes
    else cfg
