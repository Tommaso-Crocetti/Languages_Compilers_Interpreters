(* Slight changes to chaitlin algorithm *)
(* Management of rin and rout colored, check (also if rin and rout have same color) *)
(* Check coloring strategies *)
(* Manage spilling of rin, rout*)
(* Add flag for deactivating liveness analysis *)

open Mini_CFG
open Mini_RISC
open Mini_RISC_CFG
open Mini_Dataflow
open Mini_RISC_LiveRegs
module IMap = Mini_Modules.IMap
module SMap = Mini_Modules.SMap

module RMap = Map.Make (struct
  type t = reg

  let compare = compare
end)

type vertex = {
  reg : reg;
  degree : int;
  cost : int;
  color : int;
  spilled : bool;
}

type interference_graph = RSet.t RMap.t
type address_map = int RMap.t

module VertexStack = struct
  type t = vertex Stack.t

  let create () : t = Stack.create ()
  let push (v : vertex) (s : t) : unit = Stack.push v s
  let pop (s : t) : vertex = Stack.pop s
  let is_empty (s : t) : bool = Stack.is_empty s
end

module VSetDegree = Set.Make (struct
  type t = vertex

  let compare v1 v2 =
    if v1.degree <> v2.degree then compare v1.degree v2.degree
    else compare v1.reg v2.reg
end)

module VSetCost = Set.Make (struct
  type t = vertex

  let compare v1 v2 =
    if v1.cost <> v2.cost then compare v1.cost v2.cost
    else compare v1.reg v2.reg
end)

module VSetColor = Set.Make (struct
  type t = vertex

  let compare v1 v2 =
    if v1.color <> v2.color then compare v1.color v2.color
    else compare v1.reg v2.reg
end)

let compute_cost (df_risc_cfg : dataflow_risc_cfg) (reg : reg) : int =
  let used_defined_map =
    IMap.map
      (fun df_node -> find_used_defined_regs df_node.instructions)
      df_risc_cfg.nodes
  in
  IMap.fold
    (fun node_id dataflow_node acc ->
      let instructions = dataflow_node.instructions in
      let reg_usage =
        List.fold_left
          (fun usage instr ->
            let used, defined = IMap.find node_id used_defined_map in
            if RSet.mem reg used then usage + 1
            else if RSet.mem reg defined then usage + 1
            else usage)
          0 instructions
      in
      acc + reg_usage)
    df_risc_cfg.nodes 0

let registers_in_block (instrs : instruction list) : RSet.t =
  List.fold_left
    (fun acc instr ->
      let acc =
        match find_defined_reg instr with
        | Some r -> RSet.add r acc
        | None -> acc
      in
      RSet.union acc (find_used_regs instr))
    RSet.empty instrs

let registers_in_cfg (cfg : dataflow_risc_cfg) : RSet.t =
  IMap.fold
    (fun _ df_node acc ->
      RSet.union acc (registers_in_block df_node.instructions))
    cfg.nodes RSet.empty

let extend_reg_map (df_risc_cfg : dataflow_risc_cfg) (base_map : var_to_reg) :
    var_to_reg =
  let regs = registers_in_cfg df_risc_cfg in
  RSet.fold
    (fun reg acc ->
      match reg with
      | Rin | Rout -> acc
      | Ra | Rb ->
          raise
            (Error
               "Reserved temporary registers Ra/Rb cannot be used in this phase")
      | RVar id ->
          let name = Printf.sprintf "t%d" id in
          if SMap.mem name acc then acc else SMap.add name reg acc)
    regs base_map

let initialize_interference_graph (df_risc_cfg : dataflow_risc_cfg)
    (var_to_reg : var_to_reg) : interference_graph * var_to_reg =
  let extended_map = extend_reg_map df_risc_cfg var_to_reg in
  let initial_graph =
    SMap.fold
      (fun _ reg acc -> RMap.add reg RSet.empty acc)
      extended_map RMap.empty
  in
  (initial_graph, extended_map)

let add_interference_edge (graph : interference_graph) (reg1 : reg) (reg2 : reg)
    : interference_graph =
  if reg1 = reg2 then graph
  else
    let neighbors =
      match RMap.find_opt reg1 graph with
      | Some n1 -> RSet.add reg2 n1
      | None -> RSet.singleton reg2
    in
    RMap.add reg1 neighbors graph

let compute_live_ranges (df_risc_cfg : dataflow_risc_cfg)
    (graph : interference_graph) =
  IMap.fold
    (fun _ df_node acc_graph ->
      let _, updated_graph =
        List.fold_right
          (fun instr (current_live_out_regs, curr_graph) ->
            let used_regs = find_used_regs instr in
            match find_defined_reg instr with
            | Some def_reg ->
                let new_live_out =
                  RSet.union used_regs
                    (RSet.remove def_reg current_live_out_regs)
                in
                let new_graph =
                  RSet.fold
                    (fun live_reg graph ->
                      add_interference_edge
                        (add_interference_edge graph def_reg live_reg)
                        live_reg def_reg)
                    current_live_out_regs curr_graph
                in
                (new_live_out, new_graph)
            | None -> (RSet.union used_regs current_live_out_regs, curr_graph))
          df_node.instructions
          (df_node.out_regs, acc_graph)
      in
      updated_graph)
    df_risc_cfg.nodes graph

let implicit_remove_vertex (int_graph : interference_graph) (vertex : vertex)
    (curr_reg_set : VSetDegree.t) : VSetDegree.t =
  VSetDegree.filter_map
    (fun v ->
      if v.reg = vertex.reg then None
      else
        match RSet.find_opt v.reg (RMap.find v.reg int_graph) with
        | Some _ ->
            if v.degree > 0 then Some { v with degree = v.degree - 1 } else None
        | None -> Some v)
    curr_reg_set

let rec retrieve_best_reg (vertex_by_cost : VSetCost.t) (curr_reg_set : reg_set)
    : vertex * VSetCost.t =
  let best_vertex_opt = VSetCost.min_elt_opt vertex_by_cost in
  match best_vertex_opt with
  | Some best_vertex ->
      if RSet.mem best_vertex.reg curr_reg_set then (best_vertex, vertex_by_cost)
      else
        retrieve_best_reg
          (VSetCost.remove best_vertex vertex_by_cost)
          curr_reg_set
  | None -> raise (Failure "No vertex available in vertex_by_cost")

let rec push_by_degree (int_graph : interference_graph)
    (vertex_stack : VertexStack.t) (curr_reg_set : RSet.t)
    (vertex_by_degree : VSetDegree.t) (vertex_by_cost : VSetCost.t)
    (vertex_by_color : VSetColor.t) (curr_spilled_regs : RSet.t) (max_reg : int)
    : VSetColor.t * reg_set =
  let next = VSetDegree.min_elt_opt vertex_by_degree in
  match next with
  | Some vertex ->
      if vertex.degree < max_reg then (
        VertexStack.push vertex vertex_stack;
        let new_curr_reg_set = RSet.remove vertex.reg curr_reg_set in
        let new_vertex_by_degree =
          implicit_remove_vertex int_graph vertex vertex_by_degree
        in
        push_by_degree int_graph vertex_stack new_curr_reg_set
          new_vertex_by_degree vertex_by_cost vertex_by_color curr_spilled_regs
          max_reg)
      else
        push_by_cost int_graph vertex_stack curr_reg_set vertex_by_degree
          vertex_by_cost vertex_by_color curr_spilled_regs max_reg
  | None ->
      try_color int_graph vertex_stack curr_reg_set vertex_by_degree
        vertex_by_cost vertex_by_color curr_spilled_regs max_reg

and push_by_cost (int_graph : interference_graph) (vertex_stack : VertexStack.t)
    (curr_reg_set : RSet.t) (vertex_by_degree : VSetDegree.t)
    (vertex_by_cost : VSetCost.t) (vertex_by_color : VSetColor.t)
    (curr_spilled_regs : RSet.t) (max_reg : int) : VSetColor.t * reg_set =
  let next, updated_vertex_by_cost =
    retrieve_best_reg vertex_by_cost curr_reg_set
  in
  VertexStack.push next vertex_stack;
  let new_curr_reg_set = RSet.remove next.reg curr_reg_set in
  let new_vertex_by_degree =
    implicit_remove_vertex int_graph next vertex_by_degree
  in
  push_by_degree int_graph vertex_stack new_curr_reg_set new_vertex_by_degree
    updated_vertex_by_cost vertex_by_color curr_spilled_regs max_reg

and try_color (int_graph : interference_graph) (vertex_stack : VertexStack.t)
    (curr_reg_set : RSet.t) (vertex_by_degree : VSetDegree.t)
    (vertex_by_cost : VSetCost.t) (vertex_by_color : VSetColor.t)
    (curr_spilled_regs : RSet.t) (max_reg : int) : VSetColor.t * reg_set =
  match VertexStack.is_empty vertex_stack with
  | true -> (vertex_by_color, curr_spilled_regs)
  | false ->
      let next = VertexStack.pop vertex_stack in
      let neighbours = RMap.find next.reg int_graph in
      let final_color, valid =
        VSetColor.fold
          (fun v (curr_color, valid) ->
            match valid with
            | false -> (curr_color, false)
            | true -> (
                match RSet.mem v.reg neighbours with
                | true ->
                    if v.color < curr_color then (curr_color, true)
                    else if v.color = curr_color then (curr_color + 1, true)
                    else (curr_color, false)
                | false -> (curr_color, valid)))
          vertex_by_color (0, true)
      in
      if valid && final_color < max_reg then
        let colored_vertex = { next with color = final_color } in
        let new_vertex_by_color =
          VSetColor.add colored_vertex (VSetColor.remove next vertex_by_color)
        in
        try_color int_graph vertex_stack curr_reg_set vertex_by_degree
          vertex_by_cost new_vertex_by_color curr_spilled_regs max_reg
      else
        let new_curr_reg_set = RSet.remove next.reg curr_reg_set in
        let new_spilled_regs = RSet.add next.reg curr_spilled_regs in
        let new_vertex_by_color = VSetColor.remove next vertex_by_color in
        push_by_degree int_graph vertex_stack new_curr_reg_set vertex_by_degree
          vertex_by_cost new_vertex_by_color new_spilled_regs max_reg

let color_cfg (cfg : dataflow_risc_cfg) (color_map : reg RMap.t) :
    dataflow_risc_cfg =
  let colored_nodes =
    IMap.map
      (fun df_node ->
        let colored_instructions =
          List.map
            (fun instr ->
              match instr with
              | Nop | Jump _ -> instr
              | Brop (op, r1, r2, r3) ->
                  Brop
                    ( op,
                      RMap.find r1 color_map,
                      RMap.find r2 color_map,
                      RMap.find r3 color_map )
              | Biop (op, r1, n, r2) ->
                  Biop (op, RMap.find r1 color_map, n, RMap.find r2 color_map)
              | Urop (op, r1, r2) ->
                  Urop (op, RMap.find r1 color_map, RMap.find r2 color_map)
              | Load (r1, addr) -> Load (RMap.find r1 color_map, addr)
              | LoadI (n, r1) -> LoadI (n, RMap.find r1 color_map)
              | Store (r1, r2) ->
                  Store (RMap.find r1 color_map, RMap.find r2 color_map)
              | CJump (r, l1, l2) -> CJump (RMap.find r color_map, l1, l2))
            df_node.instructions
        in
        { df_node with instructions = colored_instructions })
      cfg.nodes
  in
  { cfg with nodes = colored_nodes }

let spill_slot_of_reg (reg : reg) : int =
  match reg with
  | Rin -> 0
  | Rout -> 1
  | RVar id -> id + 2
  | Ra | Rb ->
      raise
        (Error
           "Cannot assign a spill slot to reserved temporary registers Ra/Rb")

let build_spill_address_map (spilled_regs : RSet.t) : address_map =
  RSet.fold
    (fun reg acc ->
      let slot = spill_slot_of_reg reg in
      RMap.add reg slot acc)
    spilled_regs RMap.empty

let load_used (temp_reg : reg) (address_map : address_map)
    (spilled_regs : RSet.t) (reg : reg) : instruction list * reg =
  if RSet.mem reg spilled_regs then
    let addr = RMap.find reg address_map in
    ([ LoadI (addr, temp_reg); Load (temp_reg, temp_reg) ], temp_reg)
  else ([], reg)

let store_defined (address_map : address_map) (spilled_regs : RSet.t)
    (reg : reg) : instruction list * reg =
  if RSet.mem reg spilled_regs then
    let addr = RMap.find reg address_map in
    ([ LoadI (addr, Ra); Store (Rb, Ra) ], Rb)
  else ([], reg)

let rewrite_instruction_for_spill (address_map : address_map)
    (spilled_regs : RSet.t) (instr : instruction) =
  match instr with
  | Nop | Jump _ -> [ instr ]
  | LoadI _ -> [ instr ]
  | Brop (op, r1, r2, r3) ->
      let load1, op1 = load_used Ra address_map spilled_regs r1 in
      let load2, op2 = load_used Rb address_map spilled_regs r2 in
      let store, r3 = store_defined address_map spilled_regs r3 in
      load1 @ load2 @ (Brop (op, op1, op2, r3) :: store)
  | Biop (op, r1, imm, r2) ->
      let load1, op1 = load_used Ra address_map spilled_regs r1 in
      let store, r2 = store_defined address_map spilled_regs r2 in
      load1 @ (Biop (op, op1, imm, r2) :: store)
  | Urop (op, r1, r2) ->
      let load1, op1 = load_used Ra address_map spilled_regs r1 in
      let store, r2 = store_defined address_map spilled_regs r2 in
      load1 @ (Urop (op, op1, r2) :: store)
  | Load (r1, r2) ->
      let load1, op1 = load_used Ra address_map spilled_regs r1 in
      let store, r2 = store_defined address_map spilled_regs r2 in
      load1 @ (Load (op1, r2) :: store)
  | Store (r1, r2) ->
      let load1, op1 = load_used Ra address_map spilled_regs r1 in
      let load2, op2 = load_used Rb address_map spilled_regs r2 in
      load1 @ load2 @ [ Store (op1, op2) ]
  | CJump (r, l1, l2) ->
      let load1, op1 = load_used Ra address_map spilled_regs r in
      load1 @ [ CJump (op1, l1, l2) ]

let spill_cfg (cfg : dataflow_risc_cfg) (spilled_regs : RSet.t) :
    dataflow_risc_cfg =
  if RSet.is_empty spilled_regs then cfg
  else
    let address_map = build_spill_address_map spilled_regs in
    let spilled_nodes =
      IMap.map
        (fun df_node ->
          let spilled_instructions =
            List.concat
              (List.map
                 (rewrite_instruction_for_spill address_map spilled_regs)
                 df_node.instructions)
          in
          { df_node with instructions = spilled_instructions })
        cfg.nodes
    in
    { cfg with nodes = spilled_nodes }

let global_allocation (df_risc_cfg : dataflow_risc_cfg) (max_reg : int)
    (var_to_reg : var_to_reg) =
  if max_reg < 4 then
    raise
      (Failure
         "Register allocation impossible: max_reg must be at least 4 to
          accommodate reserved registers")
  else
    let base_graph, extended_reg_map =
      initialize_interference_graph df_risc_cfg var_to_reg
    in
    let interference_graph = compute_live_ranges df_risc_cfg base_graph in
    let vertex_by_degree, vertex_by_cost, vertex_by_color =
      RMap.fold
        (fun reg neighbors (degree_set, cost_set, color_set) ->
          let degree = RSet.cardinal neighbors in
          let cost = compute_cost df_risc_cfg reg in
          let vertex = { reg; degree; cost; color = -1; spilled = false } in
          ( VSetDegree.add vertex degree_set,
            VSetCost.add vertex cost_set,
            VSetColor.add vertex color_set ))
        interference_graph
        (VSetDegree.empty, VSetCost.empty, VSetColor.empty)
    in
    let initial_reg_set =
      SMap.fold (fun _ reg acc -> RSet.add reg acc) extended_reg_map RSet.empty
    in
    let colors, spilled_regs =
      push_by_degree interference_graph (VertexStack.create ()) initial_reg_set
        vertex_by_degree vertex_by_cost vertex_by_color RSet.empty (max_reg - 2)
    in
    let rin_color =
      VSetColor.fold
        (fun vertex acc ->
          match acc with
          | Some _ -> acc
          | None when vertex.reg = Rin -> Some vertex.color
          | _ -> acc)
        colors None
    in
    let rout_color =
      VSetColor.fold
        (fun vertex acc ->
          match acc with
          | Some _ -> acc
          | None when vertex.reg = Rout -> Some vertex.color
          | _ -> acc)
        colors None
    in
    let final_var_to_reg =
      VSetColor.fold
        (fun vertex acc ->
          let assigned_reg =
            match vertex.reg with
            | Rin -> Rin
            | Rout -> Rout
            | _ ->
                if Some vertex.color = rin_color then Rin
                else if Some vertex.color = rout_color then Rout
                else RVar vertex.color
          in
          RMap.add vertex.reg assigned_reg acc)
        colors RMap.empty
    in
    let colored_cfg = color_cfg df_risc_cfg final_var_to_reg in
    let spilled_cfg = spill_cfg colored_cfg spilled_regs in
    (spilled_cfg, final_var_to_reg)
