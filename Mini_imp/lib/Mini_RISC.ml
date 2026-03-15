(* TODO: 
  1. Remove unnecessary result registers by reusing the final computation register
  2. Comment the code
  3. Check if @ can be avoided
  4. Think about how input and output variables names are associated with Rin and Rout
*)

module NMap = Mini_imp_CFG.NMap

module VarOrd : Map.OrderedType with type t = string = struct
  type t = string
  let compare = compare
end

module SMap = Map.Make(VarOrd)

type reg = Rin | Rout | RVar of int

type var_to_reg = reg SMap.t

type label = string

type brop = Add | Sub | Mult | And | Or | Less
type biop = AddI | SubI | MultI | AndI | OrI
type urop = Not | Copy

type instruction =
  | Nop
  | Brop of brop * reg * reg * reg
  | Biop of biop * reg * int * reg
  | Urop of urop * reg * reg
  | Load of reg * reg
  | LoadI of int * reg
  | Store of reg * reg
  | Jump of label
  | CJump of reg * label * label

type code = instruction list SMap.t

type risc_cfg = 
  {
    nodes: instruction list NMap.t;
    edges: Mini_imp_CFG.out_node NMap.t;
    initial: int;
    final: int;
  }

let fresh_reg : unit -> reg =
  let next = ref (-1) in
  fun () ->
    incr next;
    RVar !next

let initial_reg_map (input_var : Mini_imp.var) (output_var : Mini_imp.var) : var_to_reg = 
  SMap.add output_var Rout (SMap.add input_var Rin SMap.empty)

let rec translate_aexpr (e : Mini_imp.a_exp) (target_reg : reg option) (reg_map : var_to_reg) : reg * instruction list =
  match e with
  | Mini_imp.Aval n ->
      let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
      (result_reg, [LoadI (n, result_reg)])
  | Mini_imp.Var x ->
      let base_reg = SMap.find x reg_map in
      let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
      if base_reg = result_reg then (result_reg, [])
      else (result_reg, [Urop (Copy, base_reg, result_reg)])
  | Mini_imp.Plus (a1, a2) ->
    translate_commutative_aexpr Add AddI a1 a2 target_reg reg_map
  | Mini_imp.Minus (a1, a2) ->
    translate_minus_aexpr target_reg a1 a2 reg_map
  | Mini_imp.Times (a1, a2) ->
    translate_commutative_aexpr Mult MultI a1 a2 target_reg reg_map
  | Mini_imp.Of_Bool _ ->
      failwith "of_bool not supported"

and translate_commutative_aexpr (brop : brop) (biop : biop)
  (a1 : Mini_imp.a_exp) (a2 : Mini_imp.a_exp) (target_reg : reg option) (reg_map : var_to_reg)
  : reg * instruction list =
  match (a1, a2) with
  | (Mini_imp.Aval n1, Mini_imp.Aval n2) ->
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, [LoadI (n1, result_reg); Biop (biop, result_reg, n2, result_reg)])
  | (Mini_imp.Aval n, a) | (a, Mini_imp.Aval n) ->
    (match (brop, a, n) with
    | (Add, _, 0) | (Mult, _, 1) ->
      translate_aexpr a target_reg reg_map
    | (Mult, _, 0) ->
      let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
      (result_reg, [LoadI (0, result_reg)])
    | (_, Mini_imp.Var x, _) ->
      let base_reg = SMap.find x reg_map in
      let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
      (result_reg, [Biop (biop, base_reg, n, result_reg)])
    | (_, _, _) ->
      let (t, code) = translate_aexpr a None reg_map in
      let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
      (result_reg, code @ [Biop (biop, t, n, result_reg)]))
  | (Mini_imp.Var x, a2) ->
    let base_reg = SMap.find x reg_map in
    let (t2, code2) = translate_aexpr a2 None reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, code2 @ [Brop (brop, base_reg, t2, result_reg)])
  | (a1, Mini_imp.Var x) ->
    let (t1, code1) = translate_aexpr a1 None reg_map in
    let base_reg = SMap.find x reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, code1 @ [Brop (brop, t1, base_reg, result_reg)])
  | (a1, a2) ->
    let (t1, code1) = translate_aexpr a1 None reg_map in
    let (t2, code2) = translate_aexpr a2 None reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, code1 @ code2 @ [Brop (brop, t1, t2, result_reg)])

and translate_minus_aexpr (target_reg : reg option) (a1 : Mini_imp.a_exp) (a2 : Mini_imp.a_exp)
  (reg_map : var_to_reg) : reg * instruction list =
  match (a1, a2) with
  | (Mini_imp.Aval n1, Mini_imp.Aval n2) ->
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, [LoadI (n1, result_reg); Biop (SubI, result_reg, n2, result_reg)])
  | (a, Mini_imp.Aval 0) ->
    translate_aexpr a target_reg reg_map
  | (Mini_imp.Var x, Mini_imp.Aval n) ->
    let base_reg = SMap.find x reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, [Biop (SubI, base_reg, n, result_reg)])
  | (a, Mini_imp.Aval n) ->
    let (t, code) = translate_aexpr a None reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, code @ [Biop (SubI, t, n, result_reg)])
  | (Mini_imp.Aval n, Mini_imp.Var x) ->
    let const_reg = fresh_reg () in
    let base_reg = SMap.find x reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, [LoadI (n, const_reg); Brop (Sub, const_reg, base_reg, result_reg)])
  | (Mini_imp.Aval n, a) ->
    let const_reg = fresh_reg () in
    let (t, code) = translate_aexpr a None reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, [LoadI (n, const_reg)] @ code @ [Brop (Sub, const_reg, t, result_reg)])
  | (Mini_imp.Var x, a) ->
    let base_reg = SMap.find x reg_map in
    let (t, code) = translate_aexpr a None reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, code @ [Brop (Sub, base_reg, t, result_reg)])
  | (a, Mini_imp.Var x) ->
    let (t, code) = translate_aexpr a None reg_map in
    let base_reg = SMap.find x reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, code @ [Brop (Sub, t, base_reg, result_reg)])
  | (left, right) ->
    let (t1, code1) = translate_aexpr left None reg_map in
    let (t2, code2) = translate_aexpr right None reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, code1 @ code2 @ [Brop (Sub, t1, t2, result_reg)])

let rec translate_bexpr (e : Mini_imp.b_exp) (reg_map : var_to_reg): reg * instruction list =
  match e with
  | Mini_imp.Bval v ->
      let r = fresh_reg () in
      if v then (r, [LoadI (1, r)]) else (r, [LoadI (0, r)])
  | Mini_imp.And (b1, b2) ->
      (match (b1, b2) with
      | (Mini_imp.Bval false, _) | (_, Mini_imp.Bval false) ->
        let result_reg = fresh_reg () in
        (result_reg, [LoadI (0, result_reg)])
      | (Mini_imp.Bval true, b) | (b, Mini_imp.Bval true) ->
        translate_bexpr b reg_map
      | _ ->
      let (t1, code1) = translate_bexpr b1 reg_map in
      let (t2, code2) = translate_bexpr b2 reg_map in
      let result_reg = fresh_reg () in
      (result_reg, code1 @ code2 @ [Brop (And, t1, t2, result_reg)]))
  | Mini_imp.Or (b1, b2) ->
      (match (b1, b2) with
      | (Mini_imp.Bval true, _) | (_, Mini_imp.Bval true) ->
        let result_reg = fresh_reg () in
        (result_reg, [LoadI (1, result_reg)])
      | (Mini_imp.Bval false, b) | (b, Mini_imp.Bval false) ->
        translate_bexpr b reg_map
      | _ ->
        let (t1, code1) = translate_bexpr b1 reg_map in
        let (t2, code2) = translate_bexpr b2 reg_map in
        let result_reg = fresh_reg () in
        (result_reg, code1 @ code2 @ [Brop (Or, t1, t2, result_reg)]))
  | Mini_imp.Not (b) ->
      (match b with
      | Mini_imp.Bval v ->
        let result_reg = fresh_reg () in
        let n = if v then 0 else 1 in
        (result_reg, [LoadI (n, result_reg)])
      | Mini_imp.Not b' ->
        translate_bexpr b' reg_map
      | _ ->
        let (t, code) = translate_bexpr b reg_map in
        let result_reg = fresh_reg () in
        (result_reg, code @ [Urop (Not, t, result_reg)]))
  | Mini_imp.Minor (a1, a2) ->
      (match (a1, a2) with
      | (Mini_imp.Aval n1, Mini_imp.Aval n2) ->
        (* constant fold: compute at compile time *)
        let result_reg = fresh_reg () in
        (result_reg, [LoadI ((if n1 < n2 then 1 else 0), result_reg)])
      | (Mini_imp.Var x, Mini_imp.Var y) ->
        (* both variables: single instruction, no temporaries *)
        let x_reg = SMap.find x reg_map in
        let y_reg = SMap.find y reg_map in
        let result_reg = fresh_reg () in
        (result_reg, [Brop (Less, x_reg, y_reg, result_reg)])
      | (Mini_imp.Var x, Mini_imp.Aval n) ->
        (* x < n: load n into result, compare in place *)
        let x_reg = SMap.find x reg_map in
        let result_reg = fresh_reg () in
        (result_reg, [LoadI (n, result_reg); Brop (Less, x_reg, result_reg, result_reg)])
      | (Mini_imp.Aval n, Mini_imp.Var x) ->
        (* n < x: load n into result, compare in place *)
        let x_reg = SMap.find x reg_map in
        let result_reg = fresh_reg () in
        (result_reg, [LoadI (n, result_reg); Brop (Less, result_reg, x_reg, result_reg)])
      | (Mini_imp.Var x, a2) ->
        (* x < expr: no copy needed for x *)
        let x_reg = SMap.find x reg_map in
        let (t2, code2) = translate_aexpr a2 None reg_map in
        let result_reg = fresh_reg () in
        (result_reg, code2 @ [Brop (Less, x_reg, t2, result_reg)])
      | (a1, Mini_imp.Var x) ->
        (* expr < x: no copy needed for x *)
        let x_reg = SMap.find x reg_map in
        let (t1, code1) = translate_aexpr a1 None reg_map in
        let result_reg = fresh_reg () in
        (result_reg, code1 @ [Brop (Less, t1, x_reg, result_reg)])
      | (Mini_imp.Aval n, a2) ->
        (* n < expr: translate a2, reuse result_reg for the load *)
        let (t2, code2) = translate_aexpr a2 None reg_map in
        let result_reg = fresh_reg () in
        (result_reg, code2 @ [LoadI (n, result_reg); Brop (Less, result_reg, t2, result_reg)])
      | (a1, Mini_imp.Aval n) ->
        (* expr < n: translate a1, reuse result_reg for the load *)
        let (t1, code1) = translate_aexpr a1 None reg_map in
        let result_reg = fresh_reg () in
        (result_reg, code1 @ [LoadI (n, result_reg); Brop (Less, t1, result_reg, result_reg)])
      | (a1, a2) ->
        let (t1, code1) = translate_aexpr a1 None reg_map in
        let (t2, code2) = translate_aexpr a2 None reg_map in
        let result_reg = fresh_reg () in
        (result_reg, code1 @ code2 @ [Brop (Less, t1, t2, result_reg)]))

let translate_block (stmts: Mini_imp_CFG.statement list) (reg_map: var_to_reg) : instruction list =
  let (code, _) = List.fold_left (fun (acc, curr_reg_map) stmt ->
    match stmt with
    | Mini_imp_CFG.Skip -> (acc @ [Nop], curr_reg_map)
    | Mini_imp_CFG.Assign (x, a) ->
      let (x_reg, next_reg_map) =
        match SMap.find_opt x curr_reg_map with
        | Some xr -> (xr, curr_reg_map)
        | None ->
            let xr = fresh_reg () in
            (xr, SMap.add x xr curr_reg_map)
      in
      let (_, rhs_code) = translate_aexpr a (Some x_reg) curr_reg_map in
      (acc @ rhs_code, next_reg_map)
    | Mini_imp_CFG.Guard (b) ->
      let (_, code) = translate_bexpr b curr_reg_map in
      (acc @ code, curr_reg_map)
  ) ([], reg_map) stmts in
  code

let translate_cfg (g: Mini_imp_CFG.cfg) : risc_cfg =
  let reg_map = initial_reg_map "input" "output" in
  let nodes = NMap.map (fun stmts -> translate_block stmts reg_map) g.nodes in
  { nodes; edges = g.edges; initial = g.initial; final = List.hd g.final }