open Mini_imp_AST
exception Error of string

module SMap = Map.Make(String)

type state = int SMap.t

let make_program input_var output_var body =
  { input_var; output_var; body }

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
    let initial_state = SMap.add p.input_var n SMap.empty in
    let final_state = eval_c command initial_state in
    match SMap.find_opt p.output_var final_state with
    | Some v -> v
    | None -> raise (Error ("Runtime error, output variable " ^ p.output_var ^ " not found in final state."))

let run_program (program : program) =
  print_endline "=== Input ===";
  let input = read_int () in
  print_endline "=== Output ===";
  print_int (execute program input);
  print_newline ()