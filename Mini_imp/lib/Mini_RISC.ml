module VarOrd : Map.OrderedType with type t = string = struct
  type t = string
  let compare = compare
end

module SMap = Map.Make(VarOrd)

type reg = Rin | Rout | RVar of int

type var_to_reg = reg SMap.t

type label = string

type brop = Add | Sub | Mult | And | Less | Or
type biop = Addi | Subi | Andi
type urop = Not | Copy

type instructions =
  | Nop
  | Brop of brop * reg * reg * reg
  | Biop of biop * reg * int * reg
  | Urop of urop * reg * reg
  | Load of reg * reg
  | LoadI of int * reg
  | Store of reg * reg
  | Jump of label
  | CJump of reg * label * label

type code = instructions list SMap.t

let fresh_reg : unit -> reg =
  let next = ref 0 in
  fun () ->
    incr next;
    RVar !next

let initial_reg_map (input_var : Mini_imp.var) (output_var : Mini_imp.var) : var_to_reg = 
  SMap.add output_var Rout (SMap.add input_var Rin SMap.empty)

let rec translate_aexpr (e : Mini_imp.a_exp) (reg_map : var_to_reg): reg * instructions list =
  match e with
  | Mini_imp.Aval n ->
      let r = fresh_reg () in
      (r, [LoadI (n, r)])           (* result <- NextRegister(); emit(loadI, val(node), none, result) *)

  | Mini_imp.Var x ->
      let base_reg = SMap.find x reg_map in
      let addr = fresh_reg () in
      let result = fresh_reg () in
      (* t1 <- base(node) ; t2 <- NextRegister(); emit(loadI, offset(node), none, t2); result <- NextRegister(); emit(loadAO, t1, t2, result) *)
      (* Here we model offset(node) as constant 0 for simple variable load (if var directly in memory slot). *)
      (result, [LoadI (0, addr); Load (base_reg, addr); Urop (Copy, result, addr)])

  | Mini_imp.Plus (a1, a2)
  | Mini_imp.Minus (a1, a2)
  | Mini_imp.Times (a1, a2) ->
      let op =
        match e with
        | Mini_imp.Plus _ -> Add
        | Mini_imp.Minus _ -> Sub
        | Mini_imp.Times _ -> Mult
        | _ -> assert false
      in
      let (t1, code1) = translate_aexpr a1 reg_map in
      let (t2, code2) = translate_aexpr a2 reg_map in
      let result = fresh_reg () in
      (result, code1 @ code2 @ [Brop (op, result, t1, t2)])

  | Mini_imp.Of_Bool _ ->
      failwith "translate_aexpr only handles arithmetic expressions"

let rec translate_bexpr (e : Mini_imp.b_exp) (reg_map : var_to_reg): reg * instructions list =
  match e with
  | Mini_imp.Bval v ->
      let r = fresh_reg () in
      if v then (r, [LoadI (1, r)]) else (r, [LoadI (0, r)])

  | Mini_imp.And (b1, b2) ->
      let (t1, code1) = translate_bexpr b1 reg_map in
      let (t2, code2) = translate_bexpr b2 reg_map in
      let result = fresh_reg () in
      (result, code1 @ code2 @ [Brop (And, result, t1, t2)])

  | Mini_imp.Not (b) ->
      let (t, code) = translate_bexpr b reg_map in
      let result = fresh_reg () in
      (result, code @ [Urop (Not, result, t)])
  | Mini_imp.Minor (a1, a2) ->
      let (t1, code1) = translate_aexpr a1 reg_map in
      let (t2, code2) = translate_aexpr a2 reg_map in
      let result = fresh_reg () in
      (result, code1 @ code2 @ [Brop (Less, result, t1, t2)])
  | Mini_imp.Or (b1, b2) ->
      let (t1, code1) = translate_bexpr b1 reg_map in
      let (t2, code2) = translate_bexpr b2 reg_map in
      let result = fresh_reg () in
      (result, code1 @ code2 @ [Brop (Or, result, t1, t2)])


let translate_block (stmts: Mini_imp_CFG.statement list) (reg_map: var_to_reg) : instructions list =
  List.fold_left (fun acc stmt ->
    match stmt with
    | Mini_imp_CFG.Skip -> acc @ [Nop]
    | Mini_imp_CFG.Assign (x, a) ->
      let (r, code) = translate_aexpr a reg_map in
      let _reg_map = SMap.add x r reg_map in
      acc @ code (* Keep translated result in register r; store semantics can be added if needed *)
    | Mini_imp_CFG.Guard (b) ->
      let (r, code) = translate_bexpr b reg_map in
      acc @ code
    | _ -> failwith "Only Skip and Assign are implemented in this example"
  ) [] stmts