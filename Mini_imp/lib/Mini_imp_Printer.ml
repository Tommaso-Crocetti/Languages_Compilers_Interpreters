let string_of_token (tok : Mini_imp_Parser.token) : string =
  match tok with
  | Mini_imp_Parser.DEF -> "DEF"
  | Mini_imp_Parser.MAIN -> "MAIN"
  | Mini_imp_Parser.WITH -> "WITH"
  | Mini_imp_Parser.INPUT -> "INPUT"
  | Mini_imp_Parser.OUTPUT -> "OUTPUT"
  | Mini_imp_Parser.AS -> "AS"
  | Mini_imp_Parser.INT n -> "INT(" ^ string_of_int n ^ ")"
  | Mini_imp_Parser.BOOL b -> "BOOL(" ^ string_of_bool b ^ ")"
  | Mini_imp_Parser.VAR x -> "VAR(" ^ x ^ ")"
  | Mini_imp_Parser.PLUS -> "PLUS"
  | Mini_imp_Parser.MINUS -> "MINUS"
  | Mini_imp_Parser.TIMES -> "TIMES"
  | Mini_imp_Parser.OF_BOOL -> "OF_BOOL"
  | Mini_imp_Parser.AND -> "AND"
  | Mini_imp_Parser.OR -> "OR"
  | Mini_imp_Parser.NOT -> "NOT"
  | Mini_imp_Parser.MINOR -> "MINOR"
  | Mini_imp_Parser.SKIP -> "SKIP"
  | Mini_imp_Parser.ASSIGN -> "ASSIGN"
  | Mini_imp_Parser.CONCAT -> "CONCAT"
  | Mini_imp_Parser.IF -> "IF"
  | Mini_imp_Parser.THEN -> "THEN"
  | Mini_imp_Parser.ELSE -> "ELSE"
  | Mini_imp_Parser.WHILE -> "WHILE"
  | Mini_imp_Parser.DO -> "DO"
  | Mini_imp_Parser.LPAREN -> "LPAREN"
  | Mini_imp_Parser.RPAREN -> "RPAREN"
  | Mini_imp_Parser.EOF -> "EOF"

let string_of_cfg_statement (stmt: Mini_imp_CFG.statement) : string =
  match stmt with
  | Mini_imp_CFG.Skip -> "Skip"
  | Mini_imp_CFG.Assign (x, a) -> "Assign(" ^ x ^ ", " ^ Mini_imp.aexp_to_string a ^ ")"
  | Mini_imp_CFG.Guard b -> "Guard(" ^ Mini_imp.bexp_to_string b ^ ")"

let string_of_cfg_out_node (out_node: Mini_imp_CFG.out_node) : string =
  match out_node with
  | Mini_imp_CFG.Single n -> string_of_int n
  | Mini_imp_CFG.Pair (id1, id2) -> "(" ^ string_of_int id1 ^ ", " ^ string_of_int id2 ^ ")"

let cfg_to_string (cfg: Mini_imp_CFG.cfg) : string =
  let node_to_string (node_id, stmts) =
    let stmts_str = List.map string_of_cfg_statement stmts |> String.concat "; " in
    string_of_int node_id ^ ": " ^ stmts_str
  in
  let edge_to_string (src, dst) =
    string_of_int src ^ " -> " ^ string_of_cfg_out_node dst
  in
  let nodes_str =
    Mini_imp_CFG.NMap.bindings cfg.nodes
    |> List.map node_to_string
    |> String.concat "\n"
  in
  let edges_str =
    Mini_imp_CFG.NMap.bindings cfg.edges
    |> List.map edge_to_string
    |> String.concat "\n"
  in
  "Nodes:\n" ^ nodes_str ^ "\nEdges:\n" ^ edges_str

let string_of_risc_reg (reg: Mini_RISC_CFG.reg) : string =
  match reg with
  | Mini_RISC_CFG.Rin -> "Rin"
  | Mini_RISC_CFG.Rout -> "Rout"
  | Mini_RISC_CFG.Ra -> "Ra"
  | Mini_RISC_CFG.Rb -> "Rb"
  | Mini_RISC_CFG.RVar n -> Printf.sprintf "R%d" n

let string_of_risc_brop (brop: Mini_RISC_CFG.brop) : string =
  match brop with
  | Mini_RISC_CFG.Add -> "Add"
  | Mini_RISC_CFG.Sub -> "Sub"
  | Mini_RISC_CFG.Mult -> "Mult"
  | Mini_RISC_CFG.And -> "And"
  | Mini_RISC_CFG.Or -> "Or"
  | Mini_RISC_CFG.Less -> "Less"

let string_of_risc_biop (biop: Mini_RISC_CFG.biop) : string =
  match biop with
  | Mini_RISC_CFG.AddI -> "AddI"
  | Mini_RISC_CFG.SubI -> "SubI"
  | Mini_RISC_CFG.MultI -> "MultI"
  | Mini_RISC_CFG.AndI -> "AndI"
  | Mini_RISC_CFG.OrI -> "OrI"

let string_of_risc_urop (urop: Mini_RISC_CFG.urop) : string =
  match urop with
  | Mini_RISC_CFG.Not -> "Not"
  | Mini_RISC_CFG.Copy -> "Copy"

let string_of_risc_instruction (instr: Mini_RISC_CFG.instruction) : string =
  match instr with
  | Mini_RISC_CFG.Nop -> "Nop"
  | Mini_RISC_CFG.Brop (b, r1, r2, r3) ->
    Printf.sprintf "%s %s, %s, %s"
      (string_of_risc_brop b)
      (string_of_risc_reg r1)
      (string_of_risc_reg r2)
      (string_of_risc_reg r3)
  | Mini_RISC_CFG.Biop (b, r, n, r2) ->
    Printf.sprintf "%s %s, %d, %s"
      (string_of_risc_biop b)
      (string_of_risc_reg r)
      n
      (string_of_risc_reg r2)
  | Mini_RISC_CFG.Urop (u, r1, r2) ->
    Printf.sprintf "%s %s, %s"
      (string_of_risc_urop u)
      (string_of_risc_reg r1)
      (string_of_risc_reg r2)
  | Mini_RISC_CFG.Load (r1, r2) ->
    Printf.sprintf "Load %s, %s" (string_of_risc_reg r1) (string_of_risc_reg r2)
  | Mini_RISC_CFG.LoadI (n, r) ->
    Printf.sprintf "LoadI %d, %s" n (string_of_risc_reg r)
  | Mini_RISC_CFG.Store (r1, r2) ->
    Printf.sprintf "Store %s, %s" (string_of_risc_reg r1) (string_of_risc_reg r2)
  | Mini_RISC_CFG.Jump l ->
    Printf.sprintf "Jump %s" l
  | Mini_RISC_CFG.CJump (r, l1, l2) ->
    Printf.sprintf "CJump %s, %s, %s" (string_of_risc_reg r) l1 l2

let risc_cfg_to_string (cfg: Mini_RISC_CFG.risc_cfg) : string =
  let node_to_string (node_id, node) =
    let instrs_str =
      List.map string_of_risc_instruction node.Mini_RISC_CFG.instructions
      |> String.concat "\n  "
    in
    Printf.sprintf "Node %d:\n  %s" node_id instrs_str
  in
  let edge_to_string (src, dst) =
    let dst_str =
      match dst with
      | Mini_imp_CFG.Single n -> Printf.sprintf "Single %d" n
      | Mini_imp_CFG.Pair (n1, n2) -> Printf.sprintf "Pair (%d, %d)" n1 n2
    in
    Printf.sprintf "Edge from %d to %s" src dst_str
  in
  let nodes_str =
    Mini_RISC_CFG.NMap.bindings cfg.nodes
    |> List.map node_to_string
    |> String.concat "\n\n"
  in
  let edges_str =
    Mini_RISC_CFG.NMap.bindings cfg.edges
    |> List.map edge_to_string
    |> String.concat "\n"
  in
  Printf.sprintf "Initial: %d\nFinal: %d\n\nNodes:\n%s\n\nEdges:\n%s"
    cfg.initial
    cfg.final
    nodes_str
    edges_str