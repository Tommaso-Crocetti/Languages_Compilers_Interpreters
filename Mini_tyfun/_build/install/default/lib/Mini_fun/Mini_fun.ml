type var = string

module VarOrd : Map.OrderedType with type t = var = struct
  type t = var
  let compare = compare
end

module SMap = Map.Make(VarOrd)

type op = 
  | Plus
  | Minus
  | Times
  | And
  | Or
  | Minor
and term =
  | Int of int
  | Bool of bool
  | Var of var
  | Fun of var * term
  | App of term * term
  | Op of op * term * term
  | Not of term
  | If of term * term * term
  | Let of var * term * term
  | LetFun of var * var * term * term
    
type runtime_value =
  | IntV of int
  | BoolV of bool
  | ClosureV of var * term * env
  | RecClosureV of var * var * term * env
and env = runtime_value SMap.t

let plus = Plus
let minus = Minus 
let times = Times

let and_ = And
let or_ = Or
let minor = Minor

let int_ n = Int n
let bool_ b = Bool b
let var x = Var x
let fun_ x body = Fun (x, body)
let app f arg = App (f, arg)
let op o t1 t2 = Op (o, t1, t2)
let not_ t = Not t
let if_ c t e = If (c, t, e)
let let_ x v b = Let (x, v, b)
let letfun f x body b = LetFun (f, x, body, b)

let rec eval_t (t: term) (env: env) : runtime_value =
  match t with
  | Int n -> IntV n
  | Bool b -> BoolV b
  | Var x -> 
    (match SMap.find_opt x env with
      | Some v -> v
      | None -> failwith ("Variable " ^ x ^ " not found in environment."))
  | Fun (x, body) -> ClosureV (x, body, env)
  | App (f, arg) ->
      let f_val = eval_t f env in
      let arg_val = eval_t arg env in
      (match f_val with
       | ClosureV (x, body, closure_env) ->
          let new_env = SMap.add x arg_val closure_env in
          eval_t body new_env
       | RecClosureV (f_name, x, body, closure_env) ->
          let rec_env = SMap.add f_name f_val closure_env in
          let new_env = SMap.add x arg_val rec_env in
          eval_t body new_env
       | _ -> failwith "Attempting to apply a non-function value.")
  | Op (op, t1, t2) ->
    let v1 = eval_t t1 env in
    let v2 = eval_t t2 env in
    (match (op, v1, v2) with
     | (Plus, IntV n1, IntV n2) -> IntV (n1 + n2)
     | (Minus, IntV n1, IntV n2) -> IntV (n1 - n2)
     | (Times, IntV n1, IntV n2) -> IntV (n1 * n2)
     | (And, BoolV v1, BoolV v2) -> BoolV (v1 && v2)
     | (Or, BoolV v1, BoolV v2) -> BoolV (v1 || v2)
     | (Minor, IntV n1, IntV n2) -> BoolV (n1 < n2)
     | _ -> failwith "Type error")
  | Not t1 ->
    (match eval_t t1 env with
     | BoolV v -> BoolV (not v)
     | _ -> failwith "Type error")
  | If (c, t, e) ->
      let c_val = eval_t c env in
      (match c_val with
      | BoolV true -> eval_t t env
      | BoolV false -> eval_t e env
      | _ -> failwith ("Type error: expected a boolean in if condition."))
  | Let (x, v, b) ->
      let v_val = eval_t v env in
      let new_env = SMap.add x v_val env in
      eval_t b new_env
  | LetFun (f, x, body, b) ->
      let rec_closure = RecClosureV (f, x, body, env) in
      let new_env = SMap.add f rec_closure env in
      eval_t b new_env

let compute (t: term) : runtime_value =
  eval_t t SMap.empty

let extract_int (v: runtime_value) : int =
  match v with
  | IntV n -> n
  | _ -> failwith "Expected an integer value."