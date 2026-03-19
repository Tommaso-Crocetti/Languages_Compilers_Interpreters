open Mini_imp_Parser
open Mini_imp_AST
open Mini_CFG
open Mini_imp_CFG
open Mini_RISC
open Mini_RISC_CFG

let string_of_token (tok : token) : string =
  match tok with
  | DEF -> "DEF"
  | MAIN -> "MAIN"
  | WITH -> "WITH"
  | INPUT -> "INPUT"
  | OUTPUT -> "OUTPUT"
  | AS -> "AS"
  | INT n -> "INT(" ^ string_of_int n ^ ")"
  | BOOL b -> "BOOL(" ^ string_of_bool b ^ ")"
  | VAR x -> "VAR(" ^ x ^ ")"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | TIMES -> "TIMES"
  | OF_BOOL -> "OF_BOOL"
  | AND -> "AND"
  | OR -> "OR"
  | NOT -> "NOT"
  | MINOR -> "MINOR"
  | SKIP -> "SKIP"
  | ASSIGN -> "ASSIGN"
  | CONCAT -> "CONCAT"
  | IF -> "IF"
  | THEN -> "THEN"
  | ELSE -> "ELSE"
  | WHILE -> "WHILE"
  | DO -> "DO"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | EOF -> "EOF"

let string_of_position (pos: Lexing.position) : string =
  Printf.sprintf "line %d, column %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)

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
    "def main with input " ^ p.input_var ^ " output " ^ p.output_var ^ " as\n" ^ (command_to_string p.body)

let string_of_cfg_statement (stmt: statement) : string =
  match stmt with
  | Skip -> "Skip"
  | Assign (x, a) -> "Assign(" ^ x ^ ", " ^ aexp_to_string a ^ ")"
  | Guard b -> "Guard(" ^ bexp_to_string b ^ ")"

let string_of_cfg_out_node (out_node: out_node) : string =
  match out_node with
  | Single n -> string_of_int n
  | Pair (id1, id2) -> "(" ^ string_of_int id1 ^ ", " ^ string_of_int id2 ^ ")"

let generic_cfg_to_string
  (node_to_string : int -> 'a -> string)
  (cfg : 'a generic_cfg)
  : string =
  let nodes_str =
    String.concat "\n"
      (List.map (fun (node_id, node) -> node_to_string node_id node) (IMap.bindings cfg.nodes))
  in
  let edges_str =
    String.concat "\n"
      (List.map
         (fun (node_id, out_node) ->
           string_of_int node_id ^ " -> " ^ string_of_cfg_out_node out_node)
         (IMap.bindings cfg.edges))
  in
  "Nodes:\n" ^ nodes_str ^ "\nEdges:\n" ^ edges_str

let cfg_to_string (cfg: cfg) : string =
  generic_cfg_to_string
    (fun node_id (stmts, _def_vars) ->
    let stmts_str = String.concat "; " (List.map string_of_cfg_statement stmts) in
    string_of_int node_id ^ ": " ^ stmts_str)
    cfg

let string_of_risc_reg (reg: reg) : string =
  match reg with
  | Rin -> "in"
  | Rout -> "out"
  | Ra -> "ra"
  | Rb -> "rb"
  | RVar n -> Printf.sprintf "r%d" n

let string_of_risc_brop (brop: brop) : string =
  match brop with
  | Add -> "add"
  | Sub -> "sub"
  | Mult -> "mult"
  | And -> "and"
  | Or -> "or"
  | Less -> "less"

let string_of_risc_biop (biop: biop) : string =
  match biop with
  | AddI -> "addI"
  | SubI -> "subI"
  | MultI -> "multI"
  | AndI -> "andI"
  | OrI -> "orI"

let string_of_risc_urop (urop: urop) : string =
  match urop with
  | Not -> "not"
  | Copy -> "copy"

let string_of_risc_instruction (instr: instruction) : string =
  match instr with
  | Nop -> "Nop"
  | Brop (b, r1, r2, r3) ->
    Printf.sprintf "%s %s %s => %s"
      (string_of_risc_brop b)
      (string_of_risc_reg r1)
      (string_of_risc_reg r2)
      (string_of_risc_reg r3)
  | Biop (b, r, n, r2) ->
    Printf.sprintf "%s %s %d => %s"
      (string_of_risc_biop b)
      (string_of_risc_reg r)
      n
      (string_of_risc_reg r2)
  | Urop (u, r1, r2) ->
    Printf.sprintf "%s %s => %s"
      (string_of_risc_urop u)
      (string_of_risc_reg r1)
      (string_of_risc_reg r2)
  | Load (r1, r2) ->
    Printf.sprintf "Load %s => %s" (string_of_risc_reg r1) (string_of_risc_reg r2)
  | LoadI (n, r) ->
    Printf.sprintf "LoadI %d => %s" n (string_of_risc_reg r)
  | Store (r1, r2) ->
    Printf.sprintf "Store %s => %s" (string_of_risc_reg r1) (string_of_risc_reg r2)
  | Jump l ->
    Printf.sprintf "Jump %s" l
  | CJump (r, l1, l2) ->
    Printf.sprintf "CJump %s %s %s" (string_of_risc_reg r) l1 l2

let risc_cfg_to_string (cfg: risc_cfg) : string =
  generic_cfg_to_string
    (fun node_id (instrs)->
    let instrs_str = String.concat "; " (List.map string_of_risc_instruction instrs) in
    string_of_int node_id ^ ": " ^ instrs_str)
    cfg

let reg_map_to_string (reg_map: var_to_reg) : string =
  String.concat ", " (List.map (fun (var, reg) -> Printf.sprintf "%s -> %s" var (string_of_risc_reg reg)) (SMap.bindings reg_map))

let var_set_to_string (s: var_set) : string =
  let elems = SSet.elements s in
  "{" ^ String.concat ", " elems ^ "}"

let risc_cfg_and_reg_map_to_string
  (risc_cfg : risc_cfg)
  (final_reg_map : var_to_reg)
  : string =
  Printf.sprintf
    "%s\nFinal Register Map:\n%s"
    (risc_cfg_to_string risc_cfg)
    (reg_map_to_string final_reg_map)
