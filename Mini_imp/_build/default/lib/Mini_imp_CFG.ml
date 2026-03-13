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
  initial: int option;
  final: out_node option;
}

let fresh_id =
  let next = ref 0 in
  fun () ->
    incr next;
    !next

let empty_cfg : cfg =
  {
    nodes = NMap.empty;
    edges = EMap.empty;
    initial = None;
    final = None;
  }

let get_node (g: cfg) (id: int) : statement list =
  match NMap.find_opt id g.nodes with
  | Some stmts -> stmts
  | None -> failwith ("Node with id " ^ string_of_int id ^ " not found in CFG.")

let print_final_node (cfg: cfg) (id: int) : unit =
  print_endline ("Inserted node: " ^ string_of_int id);
  match cfg.final with
  | Some (Single n) -> print_endline ("Final node: " ^ string_of_int n);
  | Some (Pair (id1, id2)) -> print_endline ("Final pair: (" ^ string_of_int id1 ^ ", " ^ string_of_int id2 ^ ")")
  | None -> print_endline "Final node: none"

let add_node (g: cfg) (stmts: statement list) : cfg =
  let id = fresh_id () in
  let cfg = { g with nodes = NMap.add id stmts g.nodes; final = Some (Single id) } in
  print_final_node cfg id;
  cfg


let add_edge (g: cfg) (src: int) (dst: out_node) : cfg =
  { g with edges = EMap.add src dst g.edges }

let make_cfg (p: Mini_imp.program) : cfg =
    let rec dfs (c: Mini_imp.command) (g: cfg) (stmts: statement list): cfg * statement list =
      match c with
      | Mini_imp.Skip -> print_endline "Skip";
                         (g, Skip::stmts)
      | Mini_imp.Assign (x, a) -> print_endline "Assign";
                                   (g, Assign (x, a)::stmts)
      | Mini_imp.Seq (c1, c2) -> 
        let (g, stmts1) = dfs c1 g stmts in
        let (g, stmts2) = dfs c2 g stmts1 in
        (g, stmts2)
      | Mini_imp.If (b, c1, c2) ->
        print_endline "If";
        let g' = add_node g (List.rev (Guard b::stmts)) in
        let node = match g'.final with
          | Some (Single n) -> n
          | _ -> failwith "Unexpected: after adding a node, the final should be a single node."
        in
        let (g1, then_stmts) = dfs c1 g' [] in 
        let g1' = (
            if then_stmts != [] then add_node g1 (List.rev then_stmts)
            else g1
        ) in 
        let then_final_node = match g1'.final with
          | Some (Single n) -> n
          | _ -> failwith "Unexpected: after processing the then branch, the final should be a single node."
        in
        let (g2, else_stmts) = dfs c2 g1' [] in 
        let g2' = (
            if else_stmts != [] then add_node g2 (List.rev else_stmts)
            else g2
        ) in
        let g3 = add_edge g2' node (Pair (node + 1, then_final_node + 1)) in
        let g_f = {g3 with final = Some (Pair (node + 1, then_final_node + 1))} in
        (g_f, [Skip])

      | Mini_imp.While (b, c) -> failwith "TODO: implement CFG construction for while loops"



    in let cfg, final_stmts = dfs p.Mini_imp.body empty_cfg [] in
    let cfg' = if final_stmts != [] then add_node cfg (List.rev final_stmts) else cfg in
    let previous_final_node = match cfg'.final with
      | Some (Single n) -> n
      | _ -> failwith "Unexpected1: after processing the entire program, the final should be a single node."
    in 
    let final_node = match cfg'.final with
      | Some (Single n) -> n
      | _ -> failwith "Unexpected2: after processing the entire program, the final should be a single node."
    in if final_node != previous_final_node then add_edge cfg' previous_final_node (Single final_node) else cfg'