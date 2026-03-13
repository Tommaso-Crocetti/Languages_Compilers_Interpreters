type var = string

module VarOrd : Map.OrderedType with type t = var = struct
  type t = var
  let compare = compare
end

module SMap = Map.Make(VarOrd)

type state = int SMap.t

type a_exp =
  | Aval of int
  | Var of var
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
  | Assign of var * a_exp
  | Seq of command * command
  | If of b_exp * command * command
  | While of b_exp * command

type program = {
  input: var;
  output: var;
  body: command;
}

let aval n = Aval n
let var x = Var x
let of_bool b = Of_Bool b
let plus a1 a2 = Plus (a1, a2)
let minus a1 a2 = Minus (a1, a2)
let times a1 a2 = Times (a1, a2)

let bval v = Bval v
let and_ b1 b2 = And (b1, b2)
let or_ b1 b2 = Or (b1, b2)
let not_ b = Not b
let minor a1 a2 = Minor (a1, a2)

let skip = Skip
let assign x a = Assign (x, a)
let seq c1 c2 = Seq (c1, c2)
let if_ b c1 c2 = If (b, c1, c2)
let while_ b c = While (b, c)

let make_program input output body =
  { input; output; body }

let rec eval_a (a:a_exp) (s:state) : int =
  match a with
   | Aval n -> n
   | Var x ->
    (match SMap.find_opt x s with
    | Some n -> n
    | None -> failwith ("Variable " ^ x ^ " not found in state.")
    )
   | Of_Bool b -> if eval_b b s then 1 else 0
   | Plus (a1, a2) -> (eval_a a1 s) + (eval_a a2 s)
   | Minus (a1, a2) -> (eval_a a1 s) - (eval_a a2 s)
   | Times (a1, a2) -> (eval_a a1 s) * (eval_a a2 s)
and eval_b (b: b_exp) (s:state) : bool =
  match b with
   | Bval v -> v
   | And (b1, b2) -> (eval_b b1 s) && (eval_b b2 s)
   | Or (b1, b2) -> (eval_b b1 s) || (eval_b b2 s)
   | Not b1 -> not (eval_b b1 s)
   | Minor (a1, a2) -> (eval_a a1 s) < (eval_a a2 s)

let rec eval_c (c: command) (s: state): state = 
  match c with
   | Skip -> s
   | Assign (x, a) -> SMap.add x (eval_a a s) s
   | Seq (c1, c2) -> eval_c c2 (eval_c c1 s)
   | If (b, c1, c2) -> if eval_b b s then eval_c c1 s else eval_c c2 s
   | While (b, c) -> if eval_b b s then eval_c (While (b, c)) (eval_c c s) else s

let execute (p: program) (n: int): int =
    let command = p.body in
    let initial_state = SMap.add p.input n SMap.empty in
    let final_state = eval_c command initial_state in
    match SMap.find_opt p.output final_state with
    | Some v -> v
    | None -> failwith ("Output variable " ^ p.output ^ " not found in final state.")

let rec aexp_to_string (a: a_exp): string =
  match a with
  | Aval n -> string_of_int n
  | Var x -> x
  | Of_Bool b -> bexp_to_string b
  | Plus (a1, a2) -> (aexp_to_string a1) ^ " + " ^ (aexp_to_string a2)
  | Minus (a1, a2) -> (aexp_to_string a1) ^ " - " ^ (aexp_to_string a2)
  | Times (a1, a2) -> (aexp_to_string a1) ^ " * " ^ (aexp_to_string a2) 
and bexp_to_string (b: b_exp): string =
  match b with
  | Bval v -> string_of_bool v
  | And (b1, b2) -> (bexp_to_string b1) ^ " && " ^ (bexp_to_string b2)
  | Or (b1, b2) -> (bexp_to_string b1) ^ " || " ^ (bexp_to_string b2)
  | Not b1 -> "!" ^ (bexp_to_string b1)
  | Minor (a1, a2) -> (aexp_to_string a1) ^ " < " ^ (aexp_to_string a2)

let rec command_to_string (c: command): string =
  match c with
  | Skip -> "skip"
  | Assign (x, a) -> x ^ " := " ^ (aexp_to_string a)
  | Seq (c1, c2) -> "(" ^ (command_to_string c1) ^ ") ; (" ^ (command_to_string c2) ^ ")"
  | If (b, c1, c2) -> "if " ^ (bexp_to_string b) ^ " then (" ^ (command_to_string c1) ^ ") else (" ^ (command_to_string c2) ^ ")"
  | While (b, c) -> "while " ^ (bexp_to_string b) ^ " do (" ^ (command_to_string c) ^ ")"
let program_to_string (p: program): string =
    "def main with input " ^ p.input ^ " output " ^ p.output ^ " as\n" ^ (command_to_string p.body)