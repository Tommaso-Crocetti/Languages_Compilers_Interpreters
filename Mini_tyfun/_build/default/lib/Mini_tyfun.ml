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
| UnknownT of fun_type option ref

type context = fun_type SMap.t
type env = context

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
let fun_ x funtype body = Fun (x, funtype, body)
let app f arg = App (f, arg)
let op o t1 t2 = Op (o, t1, t2)
let not_ t = Not t
let if_ c t e = If (c, t, e)
let let_ x v b = Let (x, v, b)
let letfun f x funtype body b = LetFun (f, x, funtype, body, b)

let fresh_unknown () = UnknownT (ref None)

let rec resolve_type t =
  match t with
  | IntT -> IntT
  | BoolT -> BoolT
  | ArrowT (param, ret) -> ArrowT (resolve_type param, resolve_type ret)
  | UnknownT r ->
      (match !r with
       | Some t' ->
           let resolved = resolve_type t' in
           r := Some resolved;
           resolved
       | None -> t)

let rec unify_types t1 t2 =
  let t1 = resolve_type t1 in
  let t2 = resolve_type t2 in
  match t1, t2 with
  | IntT, IntT -> Some IntT
  | BoolT, BoolT -> Some BoolT
  | ArrowT (p1, r1), ArrowT (p2, r2) ->
      (match unify_types p1 p2, unify_types r1 r2 with
       | Some _, Some _ -> Some (ArrowT (resolve_type p1, resolve_type r1))
       | _ -> None)
  | UnknownT r, t
  | t, UnknownT r ->
      (match !r with
       | Some existing -> unify_types existing t
       | None ->
           r := Some t;
           Some t)
  | _ -> None

let types_compatible t1 t2 =
  match unify_types t1 t2 with
  | Some _ -> true
  | None -> false

let rec typecheck (t: term) (g: context): fun_type option =
  match t with
  | Int n -> Some (IntT)
  | Bool b -> Some (BoolT)
  | Var x -> SMap.find_opt x g
  | Fun (x, funtype, body) -> 
    (match typecheck body (SMap.add x funtype g) with
     | Some body_type -> Some (ArrowT (funtype, body_type))
     | None -> None)
  | App (f, arg) ->
    (match typecheck f g, typecheck arg g with
     | Some (ArrowT (param_type, return_type)), Some arg_type when types_compatible param_type arg_type ->
        Some return_type
     | _ -> None)
  | Op (o, t1, t2) ->
    (match typecheck t1 g, typecheck t2 g with
     | Some ty1, Some ty2 when (o = Plus || o = Minus || o = Times) && types_compatible ty1 IntT && types_compatible ty2 IntT ->
        Some IntT
     | Some ty1, Some ty2 when (o = And || o = Or) && types_compatible ty1 BoolT && types_compatible ty2 BoolT ->
        Some BoolT
     | Some ty1, Some ty2 when o = Minor && types_compatible ty1 IntT && types_compatible ty2 IntT ->
        Some BoolT
     | _ -> None)
  | Not t ->
    (match typecheck t g with
     | Some ty when types_compatible ty BoolT -> Some BoolT
     | _ -> None)
  | If (c, t, e) ->
    (match typecheck c g, typecheck t g, typecheck e g with
     | Some cond_type, Some t_type, Some e_type
       when types_compatible cond_type BoolT && types_compatible t_type e_type ->
         Some (resolve_type t_type)
     | _ -> None)
  | Let (x, v, b) ->
    (match typecheck v g with
     | Some v_type -> typecheck b (SMap.add x v_type g)
     | None -> None)
  | LetFun (f, x, param_type, body, b) ->
    let return_placeholder = fresh_unknown () in
    let fun_type = ArrowT (param_type, return_placeholder) in
    let env_with_fun = SMap.add f fun_type g in
    (match typecheck body (SMap.add x param_type env_with_fun) with
     | Some body_type when types_compatible return_placeholder body_type ->
         let resolved_return = resolve_type return_placeholder in
         (match resolved_return with
          | UnknownT _ -> None
          | _ ->
              let resolved_fun = ArrowT (resolve_type param_type, resolved_return) in
              typecheck b (SMap.add f resolved_fun g))
     | _ -> None)
let typecheck_term (t: term): fun_type option =
  match typecheck t SMap.empty with
  | Some ty ->
      let resolved = resolve_type ty in
      (match resolved with
       | UnknownT _ -> None
       | _ -> Some resolved)
  | None -> None
