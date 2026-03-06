type var = string

module VarOrd : Map.OrderedType with type t = var = struct
  type t = var
  let compare = compare
end

module SMap = Map.Make(VarOrd)

type fun_type =
  | IntT
  | BoolT
  | ArrowT of fun_type * fun_type

type context = fun_type SMap.t

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
  | Fun of var * fun_type * term
  | App of term * term
  | Op of op * term * term
  | Not of term
  | If of term * term * term
  | Let of var * term * term
  | LetFun of var * var * fun_type * term * term

let plus = Plus
let minus = Minus 
let times = Times

let and_ = And
let or_ = Or
let minor = Minor

let int_ n = Int n
let bool_ b = Bool b
let var x = Var x
let fun_ x param_type body = Fun (x, param_type, body)
let app f arg = App (f, arg)
let op o t1 t2 = Op (o, t1, t2)
let not_ t = Not t
let if_ c t e = If (c, t, e)
let let_ x v b = Let (x, v, b)
let letfun f x fun_type body b = LetFun (f, x, fun_type, body, b)

let rec typecheck (t: term) (g: context): fun_type option =
  match t with
  | Int n -> Some (IntT)
  | Bool b -> Some (BoolT)
  | Var x -> SMap.find_opt x g
  | Fun (x, param_type, body) -> 
    (match typecheck body (SMap.add x param_type g) with
     | Some body_type -> Some (ArrowT (param_type, body_type))
     | None -> None)
  | App (f, arg) ->
    (match typecheck f g, typecheck arg g with
     | Some (ArrowT (param_type, return_type)), Some arg_type when param_type = arg_type ->
        Some return_type
     | _ -> None)
  | Op (o, t1, t2) ->
    (match typecheck t1 g, typecheck t2 g with
        | Some IntT, Some IntT when o = Plus || o = Minus || o = Times -> Some IntT
        | Some BoolT, Some BoolT when o = And || o = Or -> Some BoolT
        | Some IntT, Some IntT when o = Minor -> Some BoolT
        | _ -> None)        
  | Not t ->
    (match typecheck t g with
     | Some BoolT -> Some BoolT
     | _ -> None)
  | If (c, t, e) ->
    (match typecheck c g, typecheck t g, typecheck e g with
     | Some cond_type, Some t_type, Some e_type when cond_type = BoolT && t_type = e_type -> Some t_type
     | _ -> None)
  | Let (x, v, b) ->
    (match typecheck v g with
     | Some v_type -> typecheck b (SMap.add x v_type g)
     | None -> None)
  | LetFun (f, x, fun_type, body, b) ->
    let param_type, return_type = match fun_type with
      | ArrowT (input_type, output_type) -> (input_type, output_type)
      | _ -> failwith "Expected a function type for LetFun" in
    match typecheck body (SMap.add x param_type (SMap.add f fun_type g)) with
    | Some body_type when body_type = return_type -> typecheck b (SMap.add f fun_type g)
    | _ -> None
