type a_exp =
  | Aval of int
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


let rec eval_a (a:a_exp) : int =
  match a with
   | Aval n -> n
   | Of_Bool b -> if eval_b b then 1 else 0
   | Plus (a1, a2) -> (eval_a a1) + (eval_a a2)
   | Minus (a1, a2) -> (eval_a a1) - (eval_a a2)
   | Times (a1, a2) -> (eval_a a1) * (eval_a a2)
and eval_b (b: b_exp) : bool =
  match b with
   | Bval v -> v
   | And (b1, b2) -> (eval_b b1) && (eval_b b2)
   | Or (b1, b2) -> (eval_b b1) || (eval_b b2)
   | Not b1 -> not (eval_b b1)
   | Minor (a1, a2) -> (eval_a a1) < (eval_a a2)