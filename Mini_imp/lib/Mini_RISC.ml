open Mini_imp_AST
open Mini_CFG
open Mini_imp_CFG

exception Error of string

type reg = Rin | Rout | Ra | Rb | RVar of int

module SMap = Mini_Modules.SMap

(* A mapping from variable names to registers *)
type var_to_reg = reg SMap.t

module RSet = Set.Make(struct type t = reg let compare = compare end)

type reg_set = RSet.t

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
  | Jump of string
  | CJump of reg * string * string

(* Code is a mapping from labels to lists of instructions *)
type code = instruction list SMap.t

(* Helper function to generate a fresh register *)
let fresh_reg : unit -> reg =
  let next = ref (-1) in
  fun () ->
    incr next;
    RVar !next

let default_temp_regs = [Ra; Rb]

(* Helper function to choose the result register *)
let choose_result_reg (target_reg : reg option) (temp_regs : reg list) : reg =
  match target_reg, temp_regs with
  | Some reg, _ -> reg
  | None, reg :: _ -> reg
  | None, [] -> fresh_reg ()

(* Helper function to remove a reserved register from the list of temporary registers *)
let available_temp_regs (reserved_reg : reg) (temp_regs : reg list) : reg list =
  List.filter (fun reg -> reg <> reserved_reg) temp_regs

(* Helper function to create the initial register map for input and output variables *)
let initial_reg_map (input_var : string) (output_var : string) : var_to_reg = 
  SMap.add output_var Rout (SMap.add input_var Rin SMap.empty)

(* Recursive function that translate arithmetic expressions *)
let rec translate_aexpr
  (e : a_exp)
  (target_reg : reg option)
  (temp_regs : reg list)
  (def_regs : reg_set)
  (reg_map : var_to_reg)
  : reg * instruction list * reg_set =
  let result_reg = choose_result_reg target_reg temp_regs in
  match e with
  (* Base case: Constant value, a single LoadI operation is needed *)
  | Aval n ->
    (result_reg, [LoadI (n, result_reg)], RSet.add result_reg def_regs)
  (* Base case: Variable, a single Copy operation is needed *)
  | Var x ->
    let base_reg = SMap.find x reg_map in
    if base_reg = result_reg then (result_reg, [], def_regs)
    else (result_reg, [Urop (Copy, base_reg, result_reg)], RSet.add result_reg def_regs)
  (* Inductive cases, binary operations: plus and times are managed exploiting commutativity *)
  | Plus (a1, a2) ->
    translate_commutative_aexpr Add AddI a1 a2 target_reg temp_regs def_regs reg_map
  | Minus (a1, a2) ->
    translate_minus_aexpr target_reg temp_regs a1 a2 def_regs reg_map
  | Times (a1, a2) ->
    translate_commutative_aexpr Mult MultI a1 a2 target_reg temp_regs def_regs reg_map
  | Of_Bool _ ->
    raise (Error "of_bool not supported")

(* Helper function that translates commutative arithmetic expressions *)
and translate_commutative_aexpr (brop : brop) (biop : biop)
  (a1 : a_exp) (a2 : a_exp) (target_reg : reg option) (temp_regs : reg list) (def_regs: reg_set)
  (reg_map : var_to_reg)
  : reg * instruction list * reg_set =
  let result_reg = choose_result_reg target_reg temp_regs in
  let nested_temp_regs = available_temp_regs result_reg temp_regs in
  match (a1, a2) with
  (* n1 op n2, insert the first constant inside the target register and perform a biop *)
  | (Aval n1, Aval n2) ->
    (match brop with
    | Add -> (result_reg, [LoadI (n1 + n2, result_reg)], RSet.add result_reg def_regs)
    | Mult -> (result_reg, [LoadI (n1 * n2, result_reg)], RSet.add result_reg def_regs)
    | _ -> raise (Error "unsupported commutative binary operation")
    )
  (* n op a / a op n, different cases can be considered *)
  | (Aval n, a) | (a, Aval n) ->
    (match (brop, a, n) with
    (* If the constant operand is the identity elements, translate the other operand *)
    | (Add, _, 0) | (Mult, _, 1) ->
      translate_aexpr a (Some result_reg) nested_temp_regs def_regs reg_map
    (* If the constant operand is the absorbing elements, load the absorbing value *)
    | (Mult, _, 0) ->
      (result_reg, [LoadI (0, result_reg)], RSet.add result_reg def_regs)
    (* If the other operand is a variable, a single biop operation is needed,
    possibly reordering the operands *)
    | (_, Var x, _) ->
      let base_reg = SMap.find x reg_map in
      (result_reg, [Biop (biop, base_reg, n, result_reg)], RSet.add result_reg def_regs)
    (* If the other operand is an arithmetic expression, translate it and perform a biop,
    possibly reordering the operands *)
    | (_, _, _) ->
      let (_, code, def_regs1) = translate_aexpr a (Some result_reg) nested_temp_regs def_regs reg_map in
      (result_reg, code @ [Biop (biop, result_reg, n, result_reg)], RSet.add result_reg def_regs1)
    )
  (* x op y, translate both variables and perform a brop *)
  | (Var x, Var y) ->
    let x_reg = SMap.find x reg_map in
    let y_reg = SMap.find y reg_map in
    (result_reg, [Brop (brop, x_reg, y_reg, result_reg)], RSet.add result_reg def_regs)
  (* x op a / a op x, translate the arithmetic expression and perform a brop *)
  | (Var x, a) | (a, Var x) ->
    let base_reg = SMap.find x reg_map in
    (* x := x op a / x := a op x, a should be computed in a temporary register *)
    if base_reg = result_reg then
      let temp_reg = choose_result_reg None nested_temp_regs in
      let (_, code, def_regs1) = translate_aexpr a (Some temp_reg) nested_temp_regs def_regs reg_map in
      (result_reg, code @ [Brop (brop, base_reg, temp_reg, result_reg)], RSet.add result_reg def_regs1)
    (* Otherwise, translate the arithmetic expression in the result register *)
    else
      let (_, code, def_regs1) = translate_aexpr a (Some result_reg) nested_temp_regs def_regs reg_map in
      (result_reg, code @ [Brop (brop, base_reg, result_reg, result_reg)], RSet.add result_reg def_regs1)
  (* a1 op a2, recursively translate both and perform a brop *)
  | (a1, a2) ->
    let (t1, code1, def_regs1) = translate_aexpr a1 None nested_temp_regs def_regs reg_map in
    (* Exclude t1 from available registers so code2 cannot overwrite it *)
    let code2_temp_regs = available_temp_regs t1 nested_temp_regs in
    let (_, code2, def_regs2) = translate_aexpr a2 (Some result_reg) code2_temp_regs def_regs1 reg_map in
    (result_reg, code1 @ code2 @ [Brop (brop, t1, result_reg, result_reg)], RSet.add result_reg def_regs2)
(* Helper function to translate subtraction of arithmetic expressions *)
and translate_minus_aexpr (target_reg : reg option) (temp_regs : reg list)
  (a1 : a_exp) (a2 : a_exp) (def_regs: reg_set)
  (reg_map : var_to_reg) : reg * instruction list * reg_set =
  let result_reg = choose_result_reg target_reg temp_regs in
  let nested_temp_regs = available_temp_regs result_reg temp_regs in
  match (a1, a2) with
  (* Short circuit: load n1 - n2 *)
  | (Aval n1, Aval n2) ->
    (result_reg, [LoadI (n1 - n2, result_reg)], RSet.add result_reg def_regs)
  (* _ - 0, translate the first operand *)
  | (a, Aval 0) ->
    translate_aexpr a (Some result_reg) nested_temp_regs def_regs reg_map
  (* x - n, translate the variable and perform a biop *)
  | (Var x, Aval n) ->
    let base_reg = SMap.find x reg_map in
    (result_reg, [Biop (SubI, base_reg, n, result_reg)], RSet.add result_reg def_regs)
  (* a - n, translate the arithmetic expression and perform a biop *)
  | (a, Aval n) ->
    let (_, code, def_regs1) = translate_aexpr a (Some result_reg) nested_temp_regs def_regs reg_map in
    (result_reg, code @ [Biop (SubI, result_reg, n, result_reg)], RSet.add result_reg def_regs1)
  (* n - x, load the constant and perform a brop *)
  | (Aval n, Var x) ->
    let base_reg = SMap.find x reg_map in
    (* x := n - x, load n into a temp register first *)
    if base_reg = result_reg then
      let temp_reg = choose_result_reg None nested_temp_regs in
      let def_regs1 = RSet.add temp_reg def_regs in
      (result_reg, [LoadI (n, temp_reg); Brop (Sub, temp_reg, base_reg, result_reg)], RSet.add result_reg def_regs1)
    (* Otherwise, load n into the result register *)
    else
      (result_reg, [LoadI (n, result_reg); Brop (Sub, result_reg, base_reg, result_reg)], RSet.add result_reg def_regs)
  (* n - a, load the constant, translate the arithmetic expression and perform a brop *)
  | (Aval n, a) ->
    let (t, code, def_regs1) = translate_aexpr a None nested_temp_regs def_regs reg_map in
    (result_reg, code @ [LoadI (n, result_reg) ; Brop (Sub, result_reg, t, result_reg)], RSet.add result_reg def_regs1)
  (* x - y, directly perform a brop *)
  | (Var x, Var y) ->
    let x_reg = SMap.find x reg_map in
    let y_reg = SMap.find y reg_map in
    (result_reg, [Brop (Sub, x_reg, y_reg, result_reg)], RSet.add result_reg def_regs)
  (* x - a, translate the arithmetic expression and perform a brop *)
  | (Var x, a) ->
    let base_reg = SMap.find x reg_map in
    (* x := x - a, a should be computed in a different register than the one of x *)
    if base_reg = result_reg then
      let temp_reg = choose_result_reg None nested_temp_regs in
      let (_, code, def_regs1) = translate_aexpr a (Some temp_reg) nested_temp_regs def_regs reg_map in
      (result_reg, code @ [Brop (Sub, base_reg, temp_reg, result_reg)], RSet.add result_reg def_regs1)
    (* Otherwise, compute a in result_reg *)
    else
    let (_, code, def_regs1) = translate_aexpr a (Some result_reg) nested_temp_regs def_regs reg_map in
    (result_reg, code @ [Brop (Sub, base_reg, result_reg, result_reg)], RSet.add result_reg def_regs1)
  (* a - x, translate the arithmetic expression and perform a brop *)
  | (a, Var x) ->
    let base_reg = SMap.find x reg_map in
    (* x := a - x, a should be computed in a different register than the one of x *)
    if base_reg = result_reg then
      let temp_reg = choose_result_reg None nested_temp_regs in
      let (_, code, def_regs1) = translate_aexpr a (Some temp_reg) nested_temp_regs def_regs reg_map in
      (result_reg, code @ [Brop (Sub, temp_reg, base_reg, result_reg)], RSet.add result_reg def_regs1)
    (* Otherwise, compute a in result_reg *)
    else  
      let (_, code, def_regs1) = translate_aexpr a (Some result_reg) nested_temp_regs def_regs reg_map in
      (result_reg, code @ [Brop (Sub, result_reg, base_reg, result_reg)], RSet.add result_reg def_regs1)
    (* a1 - a2, translate both operands and perform a brop *)
  | (a1, a2) ->
    let (t1, code1, def_regs1) = translate_aexpr a1 None nested_temp_regs def_regs reg_map in
    (* Exclude t1 from the temp regs available to code2 so code2 cannot overwrite it *)
    let code2_temp_regs = available_temp_regs t1 nested_temp_regs in
    let (_, code2, def_regs2) = translate_aexpr a2 (Some result_reg) code2_temp_regs def_regs reg_map in
    (result_reg, code1 @ code2 @ [Brop (Sub, t1, result_reg, result_reg)], RSet.add result_reg (RSet.union def_regs1 def_regs2))

(* Recursive function that translate boolean expressions *)
let rec translate_bexpr
  (bexp : b_exp)
  (target_reg : reg option)
  (temp_regs : reg list)
  (def_regs : reg_set)
  (reg_map : var_to_reg)
  : reg * instruction list * reg_set =
  let result_reg = choose_result_reg target_reg temp_regs in
  let nested_temp_regs = available_temp_regs result_reg temp_regs in
  match bexp with
  (* Base case: boolean value, load the corresponding immediate in the result register *)
  | Bval v ->
    if v then (result_reg, [LoadI (1, result_reg)], RSet.add result_reg def_regs) else (result_reg, [LoadI (0, result_reg)], RSet.add result_reg def_regs)
  (* Inductive cases, boolean operations *)
  (* NOTE: Partial short-circuit evaluation *)
  | And (b1, b2) ->
    (* false && _ / _ && false -> false *)
    (match (b1, b2) with
    | (Bval false, _) | (_, Bval false) ->
      (result_reg, [LoadI (0, result_reg)], RSet.add result_reg def_regs)
    (* true && _ / _ && true, translate the other operand and perform a biop *)
    | (Bval true, b) | (b, Bval true) ->
      let (_, code, def_regs) = translate_bexpr b (Some result_reg) nested_temp_regs def_regs reg_map in
      (result_reg, code @ [Biop (AndI, result_reg, 1, result_reg)], RSet.add result_reg def_regs)
    (* b1 && b2, translate both operands and perform a brop *)
    | _ ->
    let (t1, code1, def_regs1) = translate_bexpr b1 None nested_temp_regs def_regs reg_map in
    let nested_temp_regs = available_temp_regs t1 nested_temp_regs in
    let (_, code2, def_regs2) = translate_bexpr b2 (Some result_reg) nested_temp_regs def_regs1 reg_map in
    (result_reg, code1 @ code2 @ [Brop (And, t1, result_reg, result_reg)], RSet.add result_reg def_regs2)
  )
  (* NOTE: Partial short-circuit evaluation *)
  | Or (b1, b2) ->
    (match (b1, b2) with
    (* true || _ / _ || true -> true *)
    | (Bval true, _) | (_, Bval true) ->
      (result_reg, [LoadI (1, result_reg)], RSet.add result_reg def_regs)
    (* false || _ / _ || false, translate the other operand and perform a biop *)
    | (Bval false, b) | (b, Bval false) ->
      let (_, code, def_regs) = translate_bexpr b (Some result_reg) nested_temp_regs def_regs reg_map in
      (result_reg, code @ [Biop (OrI, result_reg, 0, result_reg)], RSet.add result_reg def_regs)
    (* b1 || b2, translate both operands and perform a brop *)
    | _ ->
      let (t1, code1, def_regs1) = translate_bexpr b1 None nested_temp_regs def_regs reg_map in
      let nested_temp_regs = available_temp_regs t1 nested_temp_regs in
      let (t2, code2, def_regs2) = translate_bexpr b2 None nested_temp_regs def_regs1 reg_map in
      (result_reg, code1 @ code2 @ [Brop (Or, result_reg, t2, result_reg)], RSet.add result_reg def_regs2)
    )
  | Not (b) ->
    (match b with
    (* ! short-circuit evaluation *)
    | Bval v ->
      let n = if v then 0 else 1 in
      (result_reg, [LoadI (n, result_reg)], RSet.add result_reg def_regs)
    (* !(!b) -> b, translate the inner operand *)
    | Not b' ->
      translate_bexpr b' (Some result_reg) nested_temp_regs def_regs reg_map
    (* !b, translate the inner operand and perform a urop *)
    | _ ->
      let (_, code, def_regs) = translate_bexpr b (Some result_reg) nested_temp_regs def_regs reg_map in
      (result_reg, code @ [Urop (Not, result_reg, result_reg)], RSet.add result_reg def_regs)
    )
  | Minor (a1, a2) ->
    (match (a1, a2) with
    (* < Short-circuit evaluation *)
    | (Aval n1, Aval n2) ->
      (result_reg, [LoadI ((if n1 < n2 then 1 else 0), result_reg)], RSet.add result_reg def_regs)
      (* x < n: insert n into the result register and perform a brop *)
    | (Var x, Aval n) ->
      let base_reg = SMap.find x reg_map in
      (result_reg, [LoadI (n, result_reg); Brop (Less, base_reg, result_reg, result_reg)], RSet.add result_reg def_regs)
      (* n < x: insert n into the result register and perform a brop *)
    | (Aval n, Var x) ->
      let base_reg = SMap.find x reg_map in
      (result_reg, [LoadI (n, result_reg); Brop (Less, result_reg, base_reg, result_reg)], RSet.add result_reg def_regs)
    (* n < a2, insert n in the result register and perform a brop *)
    | (Aval n, a2) ->
      let (t2, code2, def_regs2) = translate_aexpr a2 None nested_temp_regs def_regs reg_map in
      (result_reg, code2 @ [LoadI (n, result_reg); Brop (Less, result_reg, t2, result_reg)], RSet.add result_reg def_regs)
    (* a1 < n, insert n in the result register and perform a brop *)
    | (a1, Aval n) ->
      let (t1, code1, def_regs1) = translate_aexpr a1 None nested_temp_regs def_regs reg_map in
      (result_reg, code1 @ [LoadI (n, result_reg); Brop (Less, t1, result_reg, result_reg)], RSet.add result_reg def_regs)
    (* x < y, perform a brop *)
    | (Var x, Var y) ->
      let base_reg1 = SMap.find x reg_map in
      let base_reg2 = SMap.find y reg_map in
      (result_reg, [Brop (Less, base_reg1, base_reg2, result_reg)], RSet.add result_reg def_regs)
    (* x < a2, translate a2 and perform a brop *)
    | (Var x, a2) ->
      let base_reg = SMap.find x reg_map in
      let (_, code2, def_regs2) = translate_aexpr a2 (Some result_reg) nested_temp_regs def_regs reg_map in
      (result_reg, code2 @ [Brop (Less, base_reg, result_reg, result_reg)], RSet.add result_reg def_regs2)
    (* a1 < x, translate a1 and perform a brop *)
    | (a1, Var x) ->
      let base_reg = SMap.find x reg_map in
      let (_, code1, def_regs1) = translate_aexpr a1 (Some result_reg) nested_temp_regs def_regs reg_map in
      (result_reg, code1 @ [Brop (Less, result_reg, base_reg, result_reg)], RSet.add result_reg def_regs1)
    (* a1 < a2, translate both expressions and perform a brop *)
    | (a1, a2) ->
      let (t1, code1, def_regs1) = translate_aexpr a1 None nested_temp_regs def_regs reg_map in
      let (_, code2, def_regs2) = translate_aexpr a2 (Some result_reg) nested_temp_regs def_regs1 reg_map in
      (result_reg, code1 @ code2 @ [Brop (Less, t1, result_reg, result_reg)], RSet.add result_reg def_regs2)
    )
(* Function that translates a list of statements into a list of RISC instructions *)
let translate_stmts (stmts: statement list) (reg_map: var_to_reg) : instruction list * var_to_reg * reg_set =
  let rev_instruction_list, final_reg_map, final_def_regs =
    List.fold_left (fun (acc, curr_reg_map, curr_def_regs) stmt ->
      match stmt with
      (* Skip -> Nop *)
      | Skip -> ([Nop] :: acc, curr_reg_map, curr_def_regs)
      (* For assignements, translate the right-hand side,
      storing the result in the register associated to the variable *)
      | Assign (x, a) ->
        let (x_reg, next_reg_map) =
          match SMap.find_opt x curr_reg_map with
          | Some xr -> (xr, curr_reg_map)
          | None ->
              let xr = fresh_reg () in
              (xr, SMap.add x xr curr_reg_map)
        in
        let (_, rhs_code, def_regs) = translate_aexpr a (Some x_reg) default_temp_regs RSet.empty curr_reg_map in
        (rhs_code :: acc, next_reg_map, RSet.add x_reg curr_def_regs)
      (* Guards are always evaluated into Ra for the following conditional jump. *)
      | Guard (b) ->
        let (_, code, def_regs) = translate_bexpr b (Some Ra) [Rb] curr_def_regs curr_reg_map in
        (code :: acc, curr_reg_map, def_regs)
    ) ([], reg_map, RSet.empty) stmts
  in
  (List.concat (List.rev rev_instruction_list), final_reg_map, final_def_regs)