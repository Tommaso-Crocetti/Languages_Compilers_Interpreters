exception Error of string

type a_exp =
  | Aval of int
  | Var of string
  | Of_Bool of b_exp
  | Plus of a_exp * a_exp
  | Minus of a_exp * a_exp
  | Times of a_exp * a_exp

and b_exp =
  | Bval of bool
  | And of b_exp * b_exp
  | Or of b_exp * b_exp
  | Not of b_exp
  | Minor of a_exp * a_exp

type command =
  | Skip
  | Assign of string * a_exp
  | Seq of command * command
  | If of b_exp * command * command
  | While of b_exp * command

type program = { input_var : string; output_var : string; body : command }

module SMap = Mini_Modules.SMap
module SSet = Mini_Modules.SSet

type state = int SMap.t
type var_set = SSet.t

let make_program input_var output_var body = { input_var; output_var; body }

let rec eval_aexp (a : a_exp) (s : state) : int =
  match a with
  | Aval n -> n
  | Var x -> (
      match SMap.find_opt x s with
      | Some n -> n
      | None -> raise (Error ("variable " ^ x ^ " not found in state.")))
  | Of_Bool b -> if eval_bexp b s then 1 else 0
  | Plus (a1, a2) -> eval_aexp a1 s + eval_aexp a2 s
  | Minus (a1, a2) -> eval_aexp a1 s - eval_aexp a2 s
  | Times (a1, a2) -> eval_aexp a1 s * eval_aexp a2 s

and eval_bexp (b : b_exp) (s : state) : bool =
  match b with
  | Bval v -> v
  | And (b1, b2) -> eval_bexp b1 s && eval_bexp b2 s
  | Or (b1, b2) -> eval_bexp b1 s || eval_bexp b2 s
  | Not b1 -> not (eval_bexp b1 s)
  | Minor (a1, a2) -> eval_aexp a1 s < eval_aexp a2 s

let rec eval_c (c : command) (s : state) : state =
  match c with
  | Skip -> s
  | Assign (x, a) -> SMap.add x (eval_aexp a s) s
  | Seq (c1, c2) -> eval_c c2 (eval_c c1 s)
  | If (b, c1, c2) -> if eval_bexp b s then eval_c c1 s else eval_c c2 s
  | While (b, c) ->
      if eval_bexp b s then eval_c (While (b, c)) (eval_c c s) else s

let execute (p : program) (n : int) : int =
  let command = p.body in
  let initial_state = SMap.add p.input_var n SMap.empty in
  let final_state = eval_c command initial_state in
  match SMap.find_opt p.output_var final_state with
  | Some v -> v
  | None ->
      raise
        (Error ("output variable " ^ p.output_var ^ " not found in final state."))

(** Finds all variables in an arithmetic expression *)
let rec find_all_vars_aexp (a : a_exp) : var_set =
  match a with
  | Aval _ -> SSet.empty
  | Var x -> SSet.singleton x
  | Of_Bool b -> find_all_vars_bexp b
  | Plus (a1, a2) | Minus (a1, a2) | Times (a1, a2) ->
      SSet.union (find_all_vars_aexp a1) (find_all_vars_aexp a2)

(** Finds all variables in a boolean expression *)
and find_all_vars_bexp (b : b_exp) : var_set =
  match b with
  | Bval _ -> SSet.empty
  | And (b1, b2) | Or (b1, b2) ->
      SSet.union (find_all_vars_bexp b1) (find_all_vars_bexp b2)
  | Not b1 -> find_all_vars_bexp b1
  | Minor (a1, a2) -> SSet.union (find_all_vars_aexp a1) (find_all_vars_aexp a2)

(** Finds all variables in a command *)
let rec find_all_vars_command (c : command) : var_set =
  match c with
  | Skip -> SSet.empty
  | Assign (x, a) -> SSet.add x (find_all_vars_aexp a)
  | Seq (c1, c2) ->
      SSet.union (find_all_vars_command c1) (find_all_vars_command c2)
  | If (b, c1, c2) ->
      SSet.union (find_all_vars_bexp b)
        (SSet.union (find_all_vars_command c1) (find_all_vars_command c2))
  | While (b, c1) ->
      SSet.union (find_all_vars_bexp b) (find_all_vars_command c1)
