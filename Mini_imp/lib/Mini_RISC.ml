(* IMPORTANT! ASK IF BOOLEAN SHORT-CIRCUIT EVALUATION CAN BE IMPLEMENTED (unneccessary AndI?) *)

(* TODO: 
  1. Remove unnecessary result registers by reusing the final computation register
  3. Check if @ can be avoided
  4. Think about how input and output variables names are associated with Rin and Rout
  5. Check if same variable in different nodes are associated with the same register
*)

module NMap = Mini_imp_CFG.NMap

module StrOrd : Map.OrderedType with type t = string = struct
  type t = string
  let compare = compare
end

module SMap = Map.Make(StrOrd)

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
    (* Nodes are represented as int -> instruction list *)
    nodes: instruction list NMap.t;
    (* Edges are represented as int -> int list *)
    edges: Mini_imp_CFG.out_node NMap.t;
    initial: int;
    final: int;
  }


(** Helper function to generate a fresh register *)
let fresh_reg : unit -> reg =
  let next = ref (-1) in
  fun () ->
    incr next;
    RVar !next

(* Helper function to create the initial register map for input and output variables *)
let initial_reg_map (input_var : Mini_imp.var) (output_var : Mini_imp.var) : var_to_reg = 
  SMap.add output_var Rout (SMap.add input_var Rin SMap.empty)

(* Recursive function that translate arithmetic expressions *)
let rec translate_aexpr (e : Mini_imp.a_exp) (target_reg : reg option) (reg_map : var_to_reg) : reg * instruction list =
  match e with
  (* Base case: Constant value, a single LoadI operation is needed *)
  | Mini_imp.Aval n ->
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, [LoadI (n, result_reg)])
  (* Base case: Variable, a single Copy operation is needed *)
  | Mini_imp.Var x ->
    let base_reg = SMap.find x reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    if base_reg = result_reg then (result_reg, [])
    else (result_reg, [Urop (Copy, base_reg, result_reg)])
  (* Inductive cases, binary operations: plus and times are managed exploiting commutativity *)
  | Mini_imp.Plus (a1, a2) ->
    translate_commutative_aexpr Add AddI a1 a2 target_reg reg_map
  | Mini_imp.Minus (a1, a2) ->
    translate_minus_aexpr target_reg a1 a2 reg_map
  | Mini_imp.Times (a1, a2) ->
    translate_commutative_aexpr Mult MultI a1 a2 target_reg reg_map
  | Mini_imp.Of_Bool _ ->
    failwith "of_bool not supported"

(* Helper function that translates commutative arithmetic expressions *)
and translate_commutative_aexpr (brop : brop) (biop : biop)
  (a1 : Mini_imp.a_exp) (a2 : Mini_imp.a_exp) (target_reg : reg option) (reg_map : var_to_reg)
  : reg * instruction list =
  match (a1, a2) with
  (* n1 op n2, insert the first constant inside the target register and perform a biop *)
  | (Mini_imp.Aval n1, Mini_imp.Aval n2) ->
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, [LoadI (n1, result_reg); Biop (biop, result_reg, n2, result_reg)])
  (* n op a / a op n, different cases can be considered *)
  | (Mini_imp.Aval n, a) | (a, Mini_imp.Aval n) ->
    (match (brop, a, n) with
    (* If the constant operand is the identity elements, translate the other operand *)
    | (Add, _, 0) | (Mult, _, 1) ->
      translate_aexpr a target_reg reg_map
    (* If the constant operand is the absorbing elements, load the absorbing value *)
    | (Mult, _, 0) ->
      let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
      (result_reg, [LoadI (0, result_reg)])
    (* If the other operand is a variable, a single biop operation is needed,
    possibly reordering the operands *)
    | (_, Mini_imp.Var x, _) ->
      let base_reg = SMap.find x reg_map in
      let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
      (result_reg, [Biop (biop, base_reg, n, result_reg)])
    (* If the other operand is an arithmetic expression, translate it and perform a biop,
    possibly reordering the operands *)
    | (_, _, _) ->
      let (t, code) = translate_aexpr a None reg_map in
      let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
      (result_reg, code @ [Biop (biop, t, n, result_reg)])
    )
  (* x op a, translate the arithmetic expression and perform a brop *)
  | (Mini_imp.Var x, a2) ->
    let base_reg = SMap.find x reg_map in
    let (t2, code2) = translate_aexpr a2 None reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, code2 @ [Brop (brop, base_reg, t2, result_reg)])
  (* a op x, translate the arithmetic expression and perform a brop *)
  | (a1, Mini_imp.Var x) ->
    let (t1, code1) = translate_aexpr a1 None reg_map in
    let base_reg = SMap.find x reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, code1 @ [Brop (brop, t1, base_reg, result_reg)])
  (* a1 op a2, recursively translate both and perform a brop *)
  | (a1, a2) ->
    let (t1, code1) = translate_aexpr a1 None reg_map in
    let (t2, code2) = translate_aexpr a2 None reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, code1 @ code2 @ [Brop (brop, t1, t2, result_reg)])

(* Helper function to translate subtraction of arithmetic expressions *)
and translate_minus_aexpr (target_reg : reg option) (a1 : Mini_imp.a_exp) (a2 : Mini_imp.a_exp)
  (reg_map : var_to_reg) : reg * instruction list =
  match (a1, a2) with
  (* n1 - n2, insert the first constant inside the result register and perform a biop *)
  | (Mini_imp.Aval n1, Mini_imp.Aval n2) ->
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, [LoadI (n1, result_reg); Biop (SubI, result_reg, n2, result_reg)])
  (* _ - 0, translate the first operand *)
  | (a, Mini_imp.Aval 0) ->
    translate_aexpr a target_reg reg_map
  (* x - n, translate the variable and perform a biop *)
  | (Mini_imp.Var x, Mini_imp.Aval n) ->
    let base_reg = SMap.find x reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, [Biop (SubI, base_reg, n, result_reg)])
  (* a - n, translate the arithmetic expression and perform a biop *)
  | (a, Mini_imp.Aval n) ->
    let (t, code) = translate_aexpr a None reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, code @ [Biop (SubI, t, n, result_reg)])
  (* n - x, load the constant and perform a brop *)
  | (Mini_imp.Aval n, Mini_imp.Var x) ->
    let base_reg = SMap.find x reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, [LoadI (n, result_reg); Brop (Sub, result_reg, base_reg, result_reg)])
  (* n - a, load the constant, translate the arithmetic expression and perform a brop *)
  | (Mini_imp.Aval n, a) ->
    let (t, code) = translate_aexpr a None reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, [LoadI (n, result_reg)] @ code @ [Brop (Sub, result_reg, t, result_reg)])
  (* x - a, translate the arithmetic expression and perform a brop *)
  | (Mini_imp.Var x, a) ->
    let base_reg = SMap.find x reg_map in
    let (t, code) = translate_aexpr a None reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, code @ [Brop (Sub, base_reg, t, result_reg)])
  (* a - x, translate the arithmetic expression and perform a brop *)
  | (a, Mini_imp.Var x) ->
    let (t, code) = translate_aexpr a None reg_map in
    let base_reg = SMap.find x reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, code @ [Brop (Sub, t, base_reg, result_reg)])
  (* a1 - a2, translate both operands and perform a brop *)
  | (left, right) ->
    let (t1, code1) = translate_aexpr left None reg_map in
    let (t2, code2) = translate_aexpr right None reg_map in
    let result_reg = match target_reg with Some r -> r | None -> fresh_reg () in
    (result_reg, code1 @ code2 @ [Brop (Sub, t1, t2, result_reg)])

(* Recursive function that translate boolean expressions *)
let rec translate_bexpr (e : Mini_imp.b_exp) (reg_map : var_to_reg): reg * instruction list =
  match e with
  (* Base case: boolean value, load the corresponding immediate in the result register *)
  | Mini_imp.Bval v ->
    let r = fresh_reg () in
    if v then (r, [LoadI (1, r)]) else (r, [LoadI (0, r)])
  (* Inductive cases, boolean operations *)
  (* NOTE: Partial short-circuit evaluation *)
  | Mini_imp.And (b1, b2) ->
    (* false && _ / _ && false -> false *)
    (match (b1, b2) with
    | (Mini_imp.Bval false, _) | (_, Mini_imp.Bval false) ->
      let result_reg = fresh_reg () in
      (result_reg, [LoadI (0, result_reg)])
    (* true && _ / _ && true, translate the other operand and perform a biop *)
    | (Mini_imp.Bval true, b) | (b, Mini_imp.Bval true) ->
      let (t, code) = translate_bexpr b reg_map in
      let result_reg = fresh_reg () in
      (result_reg, code @ [Biop (AndI, t, 1, result_reg)])
    (* b1 && b2, translate both operands and perform a brop *)
    | _ ->
    let (t1, code1) = translate_bexpr b1 reg_map in
    let (t2, code2) = translate_bexpr b2 reg_map in
    let result_reg = fresh_reg () in
    (result_reg, code1 @ code2 @ [Brop (And, t1, t2, result_reg)])
  )
  (* NOTE: Partial short-circuit evaluation *)
  | Mini_imp.Or (b1, b2) ->
    (match (b1, b2) with
    (* true || _ / _ || true -> true *)
    | (Mini_imp.Bval true, _) | (_, Mini_imp.Bval true) ->
      let result_reg = fresh_reg () in
      (result_reg, [LoadI (1, result_reg)])
    (* false || _ / _ || false, translate the other operand and perform a biop *)
    | (Mini_imp.Bval false, b) | (b, Mini_imp.Bval false) ->
      let (t, code) = translate_bexpr b reg_map in
      let result_reg = fresh_reg () in
      (result_reg, code @ [Biop (OrI, t, 0, result_reg)])
    (* b1 || b2, translate both operands and perform a brop *)
    | _ ->
      let (t1, code1) = translate_bexpr b1 reg_map in
      let (t2, code2) = translate_bexpr b2 reg_map in
      let result_reg = fresh_reg () in
      (result_reg, code1 @ code2 @ [Brop (Or, t1, t2, result_reg)])
    )
  | Mini_imp.Not (b) ->
    (match b with
    (* ! short-circuit evaluation *)
    | Mini_imp.Bval v ->
      let result_reg = fresh_reg () in
      let n = if v then 0 else 1 in
      (result_reg, [LoadI (n, result_reg)])
    (* !(!b) -> b, translate the inner operand *)
    | Mini_imp.Not b' ->
      translate_bexpr b' reg_map
    (* !b, translate the inner operand and perform a urop *)
    | _ ->
      let (t, code) = translate_bexpr b reg_map in
      let result_reg = fresh_reg () in
      (result_reg, code @ [Urop (Not, t, result_reg)])
    )
  | Mini_imp.Minor (a1, a2) ->
    (match (a1, a2) with
    (* < Short-circuit evaluation *)
    | (Mini_imp.Aval n1, Mini_imp.Aval n2) ->
      let result_reg = fresh_reg () in
      (result_reg, [LoadI ((if n1 < n2 then 1 else 0), result_reg)])
      (* x < n: insert n into the result register and perform a brop *)
    | (Mini_imp.Var x, Mini_imp.Aval n) ->
      let x_reg = SMap.find x reg_map in
      let result_reg = fresh_reg () in
      (result_reg, [LoadI (n, result_reg); Brop (Less, x_reg, result_reg, result_reg)])
      (* n < x: insert n into the result register and perform a brop *)
    | (Mini_imp.Aval n, Mini_imp.Var x) ->
      let x_reg = SMap.find x reg_map in
      let result_reg = fresh_reg () in
      (result_reg, [LoadI (n, result_reg); Brop (Less, result_reg, x_reg, result_reg)])
    (* n < a2, insert n in the result register and perform a brop *)
    | (Mini_imp.Aval n, a2) ->
      (* n < expr: translate a2, reuse result_reg for the load *)
      let (t2, code2) = translate_aexpr a2 None reg_map in
      let result_reg = fresh_reg () in
      (result_reg, code2 @ [LoadI (n, result_reg); Brop (Less, result_reg, t2, result_reg)])
    (* a1 < n, insert n in the result register and perform a brop *)
    | (a1, Mini_imp.Aval n) ->
      (* expr < n: translate a1, reuse result_reg for the load *)
      let (t1, code1) = translate_aexpr a1 None reg_map in
      let result_reg = fresh_reg () in
      (result_reg, code1 @ [LoadI (n, result_reg); Brop (Less, t1, result_reg, result_reg)])
    (* x < y, perform a brop *)
    | (Mini_imp.Var x, Mini_imp.Var y) ->
      let x_reg = SMap.find x reg_map in
      let y_reg = SMap.find y reg_map in
      let result_reg = fresh_reg () in
      (result_reg, [Brop (Less, x_reg, y_reg, result_reg)])
    (* x < a2, translate a2 and perform a brop *)
    | (Mini_imp.Var x, a2) ->
      let x_reg = SMap.find x reg_map in
      let (t2, code2) = translate_aexpr a2 None reg_map in
      let result_reg = fresh_reg () in
      (result_reg, code2 @ [Brop (Less, x_reg, t2, result_reg)])
    (* a1 < x, translate a1 and perform a brop *)
    | (a1, Mini_imp.Var x) ->
      let x_reg = SMap.find x reg_map in
      let (t1, code1) = translate_aexpr a1 None reg_map in
      let result_reg = fresh_reg () in
      (result_reg, code1 @ [Brop (Less, t1, x_reg, result_reg)])
    (* a1 < a2, translate both expressions and perform a brop *)
    | (a1, a2) ->
      let (t1, code1) = translate_aexpr a1 None reg_map in
      let (t2, code2) = translate_aexpr a2 None reg_map in
      let result_reg = fresh_reg () in
      (result_reg, code1 @ code2 @ [Brop (Less, t1, t2, result_reg)])
    )

(* Function that translates a list of statements into a list of RISC instructions *)
let translate_block (stmts: Mini_imp_CFG.statement list) (reg_map: var_to_reg) : instruction list =
  let (code, _) = List.fold_left (fun (acc, curr_reg_map) stmt ->
    match stmt with
    (* Skip -> Nop *)
    | Mini_imp_CFG.Skip -> (acc @ [Nop], curr_reg_map)
    (* For assignements, translate the right-hand side,
    storing the result in the register associated to the variable *)
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
    (* For guards, simply translate the boolean expression *)
    | Mini_imp_CFG.Guard (b) ->
      let (_, code) = translate_bexpr b curr_reg_map in
      (acc @ code, curr_reg_map)
  ) ([], reg_map) stmts in
  code

(* Main function, translates a control flow graph to a RISC control flow graph *)
let translate_cfg (g: Mini_imp_CFG.cfg) (input_var: string) (output_var: string): risc_cfg =
  let reg_map = initial_reg_map input_var output_var in
  let nodes = NMap.map (fun stmts -> translate_block stmts reg_map) g.nodes in
  { nodes; edges = g.edges; initial = g.initial; final = List.hd g.final }