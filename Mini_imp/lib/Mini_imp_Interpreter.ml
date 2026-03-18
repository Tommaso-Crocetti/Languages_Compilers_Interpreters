exception Error of string

module SMap = Map.Make(String)

type state = int SMap.t

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

type program = {
  input: string;
  output: string;
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

let rec eval_aexp (a:a_exp) (s:state) : int =
  match a with
   | Aval n -> n
   | Var x ->
    (match SMap.find_opt x s with
    | Some n -> n
    | None -> raise (Error ("Runtime error, variable " ^ x ^ " not found in state."))
    )
   | Of_Bool b -> if eval_bexp b s then 1 else 0
   | Plus (a1, a2) -> (eval_aexp a1 s) + (eval_aexp a2 s)
   | Minus (a1, a2) -> (eval_aexp a1 s) - (eval_aexp a2 s)
   | Times (a1, a2) -> (eval_aexp a1 s) * (eval_aexp a2 s)
and eval_bexp (b: b_exp) (s:state) : bool =
  match b with
   | Bval v -> v
   | And (b1, b2) -> (eval_bexp b1 s) && (eval_bexp b2 s)
   | Or (b1, b2) -> (eval_bexp b1 s) || (eval_bexp b2 s)
   | Not b1 -> not (eval_bexp b1 s)
   | Minor (a1, a2) -> (eval_aexp a1 s) < (eval_aexp a2 s)

let rec eval_c (c: command) (s: state): state = 
  match c with
   | Skip -> s
   | Assign (x, a) -> SMap.add x (eval_aexp a s) s
   | Seq (c1, c2) -> eval_c c2 (eval_c c1 s)
   | If (b, c1, c2) -> if eval_bexp b s then eval_c c1 s else eval_c c2 s
   | While (b, c) -> if eval_bexp b s then eval_c (While (b, c)) (eval_c c s) else s

let execute (p: program) (n: int): int =
    let command = p.body in
    let initial_state = SMap.add p.input n SMap.empty in
    let final_state = eval_c command initial_state in
    match SMap.find_opt p.output final_state with
    | Some v -> v
    | None -> raise (Error ("Runtime error, output variable " ^ p.output ^ " not found in final state."))

let run_program (program : program) =
  print_endline "=== Input ===";
  let input = read_int () in
  print_endline "=== Output ===";
  print_int (execute program input);
  print_newline ()