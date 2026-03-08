open Mini_fun

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

let int_type = IntT

let bool_type = BoolT

let arrow t1 t2 = ArrowT (t1, t2)

let type_check (t: term): fun_type option =
  let rec term_type_check (t: term) (g: context): fun_type option =
    match t with
    | Int n -> Some (IntT)
    | Bool b -> Some (BoolT)
    | Var x -> SMap.find_opt x g
    | Fun (x, param_type, body) -> 
      (match term_type_check body (SMap.add x param_type g) with
      | Some body_type -> Some (ArrowT (param_type, body_type))
      | None -> None)
    | App (f, arg) ->
      (match term_type_check f g, term_type_check arg g with
      | Some (ArrowT (param_type, return_type)), Some arg_type when param_type = arg_type ->
          Some return_type
      | _ -> None)
    | Op (o, t1, t2) ->
      (match term_type_check t1 g, term_type_check t2 g with
          | Some IntT, Some IntT when o = Plus || o = Minus || o = Times -> Some IntT
          | Some BoolT, Some BoolT when o = And || o = Or -> Some BoolT
          | Some IntT, Some IntT when o = Minor -> Some BoolT
          | _ -> None)        
    | Not t ->
      (match term_type_check t g with
      | Some BoolT -> Some BoolT
      | _ -> None)
    | If (c, t, e) ->
      (match term_type_check c g, term_type_check t g, term_type_check e g with
      | Some cond_type, Some t_type, Some e_type when cond_type = BoolT && t_type = e_type -> Some t_type
      | _ -> None)
    | Let (x, v, b) ->
      (match term_type_check v g with
      | Some v_type -> term_type_check b (SMap.add x v_type g)
      | None -> None)
    | LetFun (f, x, fun_type, body, b) ->
      let param_type, return_type = match fun_type with
        | ArrowT (input_type, output_type) -> (input_type, output_type)
        | _ -> failwith "Expected a function type for LetFun" in
      match term_type_check body (SMap.add x param_type (SMap.add f fun_type g)) with
      | Some body_type when body_type = return_type -> term_type_check b (SMap.add f fun_type g)
      | _ -> None
  in term_type_check t SMap.empty

let rec drop_types (t: term): Mini_fun.term =
    let cast_op o =
      match o with
      | Plus -> Mini_fun.plus
      | Minus -> Mini_fun.minus
      | Times -> Mini_fun.times
      | And -> Mini_fun.and_
      | Or -> Mini_fun.or_
      | Minor -> Mini_fun.minor 
    in match t with
  | Int n -> Mini_fun.int_ n
  | Bool b -> Mini_fun.bool_ b
  | Var x -> Mini_fun.var x
  | Fun (x, _, body) -> Mini_fun.fun_ x (drop_types body)
  | App (f, arg) -> Mini_fun.app (drop_types f) (drop_types arg)
  | Op (o, t1, t2) -> (Mini_fun.op (cast_op o) (drop_types t1) (drop_types t2))
  | Not t -> Mini_fun.not_ (drop_types t)
  | If (c, t, e) -> Mini_fun.if_ (drop_types c) (drop_types t) (drop_types e)
  | Let (x, v, b) -> Mini_fun.let_ x (drop_types v) (drop_types b)
  | LetFun (f, x, _, body, b) -> Mini_fun.letfun f x (drop_types body) (drop_types b)

let execute (t: term) (n: int): int =
  match type_check t with
  | Some _ -> (
    Mini_fun.extract_int (Mini_fun.compute (Mini_fun.app (drop_types t) (Mini_fun.int_ n)))
  )
  | None -> failwith "Type error: term is not well-typed."