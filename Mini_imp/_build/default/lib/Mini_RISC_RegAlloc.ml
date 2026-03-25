open Mini_CFG
open Mini_RISC
open Mini_RISC_CFG
open Mini_Dataflow
open Mini_RISC_LiveRegs
module IMap = Mini_Modules.IMap
module ISet = Mini_Modules.ISet
module SMap = Mini_Modules.SMap

(** A vertex in the interference graph, annotated with degree, cost, and color *)
type vertex = { reg : reg; degree : int; cost : float; color : int }

type reg_set = RSet.t

(** Mapping from registers to their costs, according to some heuristic *)
type cost_map = int RMap.t

(** An iterference graph is a mapping from registers to the set of conflicting register *)
type interference_graph = reg_set RMap.t

(** Mapping from spilled registers to memory addresses. *)
type address_map = int RMap.t

(** Stack used during graph simplification. *)
module VertexStack = struct
  type t = vertex Stack.t

  let create () : t = Stack.create ()

  let push (v : vertex) (s : t) : unit = Stack.push v s

  let pop (s : t) : vertex = Stack.pop s

  let is_empty (s : t) : bool = Stack.is_empty s
end

type vstack = VertexStack.t

module VSetDegree = Set.Make (struct
  type t = vertex

  let compare v1 v2 =
    if v1.degree <> v2.degree then compare v1.degree v2.degree
    else compare v1.reg v2.reg
end)

type vset_degree = VSetDegree.t

module VSetCost = Set.Make (struct
  type t = vertex

  let compare v1 v2 =
    if v1.cost <> v2.cost then compare v1.cost v2.cost
    else compare v1.reg v2.reg
end)

type vset_cost = VSetCost.t

module VSetColor = Set.Make (struct
  type t = vertex

  let compare v1 v2 =
    if v1.color <> v2.color then compare v1.color v2.color
    else compare v1.reg v2.reg
end)

type vset_color = VSetColor.t

(** Mapping from virtual registers to their assigned physical registers *)
type color_map = reg RMap.t

(** Increment the cost counter associated with a register *)
let increment_cost (reg : reg) (cost_map : cost_map) : cost_map =
  let current =
    match RMap.find_opt reg cost_map with Some v -> v | None -> 0
  in
  RMap.add reg (current + 1) cost_map

(** Compute usage-based costs for all registers in the CFG *)
let compute_cost_map (risc_cfg : risc_cfg) : cost_map =
  IMap.fold
    (fun _ instrs acc ->
      List.fold_left
        (fun cost_map instr ->
          (* bump the cost for every register that participates in the instruction *)
          let regs =
            match find_defined_reg instr with
            | Some def_reg -> RSet.add def_reg (find_used_regs instr)
            | None -> find_used_regs instr
          in
          RSet.fold (fun reg map -> increment_cost reg map) regs cost_map)
        acc instrs)
    risc_cfg.nodes RMap.empty

(** Extend the variable-to-register map with temporaries discovered in the CFG *)
let extend_reg_map (risc_cfg : risc_cfg) (base_map : var_to_reg) : var_to_reg =
  let regs = registers_in_cfg risc_cfg in
  RSet.fold
    (fun reg acc ->
      match reg with
      | Ra | Rb -> acc
      | RVar id ->
          let name = Printf.sprintf "t%d" id in
          if SMap.mem name acc then acc else SMap.add name reg acc
      | Rin | Rout ->
          if SMap.exists (fun _ r -> r = reg) acc then acc
          else SMap.add (if reg = Rin then "in" else "out") reg acc)
    regs base_map

(** Initialize an empty interference graph according to the extended register map *)
let initialize_interference_graph (risc_cfg : risc_cfg)
    (var_to_reg : var_to_reg) : interference_graph * var_to_reg =
  let extended_map = extend_reg_map risc_cfg var_to_reg in
  let initial_graph =
    SMap.fold
      (fun _ reg acc -> RMap.add reg RSet.empty acc)
      extended_map RMap.empty
  in
  (initial_graph, extended_map)

(** Add an undirected interference edge between two registers *)
let add_interference_edge (int_graph : interference_graph) (reg1 : reg)
    (reg2 : reg) : interference_graph =
  if reg1 = reg2 then int_graph
  else
    let neighbors =
      match RMap.find_opt reg1 int_graph with
      | Some n1 -> RSet.add reg2 n1
      | None -> RSet.singleton reg2
  in
    RMap.add reg1 neighbors int_graph

(** Build an interference graph that assumes all registers conflict *)
let build_without_liveness (int_graph : interference_graph)
    (extended_map : var_to_reg) : interference_graph =
  let regs =
    SMap.fold (fun _ reg acc -> RSet.add reg acc) extended_map RSet.empty
  in
  RSet.fold
    (fun reg acc ->
      RMap.add reg (RSet.filter (fun curr_reg -> curr_reg <> reg) regs) acc)
    regs int_graph

(** Build the interference graph using liveness information *)
let build_with_liveness (int_graph : interference_graph)
    (df_risc_cfg : dataflow_risc_cfg) : interference_graph =
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
                    (fun live_reg int_graph ->
                      (* connect the defined register with every live-out register *)
                      add_interference_edge
                        (add_interference_edge int_graph def_reg live_reg)
                        live_reg def_reg)
                    current_live_out_regs curr_graph
                in
                (new_live_out, new_graph)
            | None -> (RSet.union used_regs current_live_out_regs, curr_graph))
          df_node.instructions
          (df_node.out_regs, acc_graph)
      in
      updated_graph)
    df_risc_cfg.nodes int_graph

(** Rebuild the interference graph and auxiliary sets after a spill *)
let init_interference_graph (int_graph : interference_graph)
    (spilled_reg : reg option) (cost_map : cost_map) :
    interference_graph * vset_degree * vset_cost * vset_color =
  let initial_new_graph =
    match spilled_reg with
    | Some reg -> RMap.remove reg int_graph
    | None -> int_graph
  in
  let new_graph =
    match spilled_reg with
    | Some spilled_reg ->
        RMap.map
          (fun neighbors -> RSet.remove spilled_reg neighbors)
          initial_new_graph
    | None -> initial_new_graph
  in
  let new_vertex_by_degree, new_vertex_by_cost, new_vertex_by_color =
    RMap.fold
      (fun reg neighbors (degree_set, cost_set, color_set) ->
        let degree = RSet.cardinal neighbors in
        let cost =
          match RMap.find_opt reg cost_map with
          | Some c ->
              if degree = 0 then 0.0 else float_of_int c /. float_of_int degree
          | None -> 0.0
        in
        let vertex = { reg; degree; cost; color = -1 } in
        ( VSetDegree.add vertex degree_set,
          VSetCost.add vertex cost_set,
          VSetColor.add vertex color_set ))
      new_graph
      (VSetDegree.empty, VSetCost.empty, VSetColor.empty)
  in
  (new_graph, new_vertex_by_degree, new_vertex_by_cost, new_vertex_by_color)

(** Remove a vertex from the auxiliary sets while updating neighbor metadata *)
let implicit_remove_vertex (int_graph : interference_graph) (vertex : vertex)
    (vertex_by_degree : vset_degree) (vertex_by_cost : vset_cost)
    (vertex_by_color : vset_color) : vset_degree * vset_cost * vset_color =
  let new_vertex_by_degree =
    VSetDegree.filter_map
      (fun v ->
        if v.reg = vertex.reg then None
        else
          match RSet.find_opt vertex.reg (RMap.find v.reg int_graph) with
          | Some _ ->
              if v.degree > 0 then Some { v with degree = v.degree - 1 }
              else None
          | None -> Some v)
      vertex_by_degree
  in
  let new_vertex_by_cost =
    VSetCost.filter (fun v -> v.reg <> vertex.reg) vertex_by_cost
  in
  let new_vertex_by_cost =
    VSetCost.map
      (fun v ->
        if RSet.mem vertex.reg (RMap.find v.reg int_graph) && v.degree > 0 then
          {
            v with
            cost =
              v.cost *. float_of_int (v.degree + 1) /. float_of_int v.degree;
          }
        else v)
      new_vertex_by_cost
  in
  let new_vertex_by_color =
    VSetColor.filter (fun v -> v.reg <> vertex.reg) vertex_by_color
  in
  (new_vertex_by_degree, new_vertex_by_cost, new_vertex_by_color)

(** Simplification step that prefers low-degree vertices *)
let rec push_by_degree (int_graph : interference_graph) (vertex_stack : vstack)
    (vertex_by_degree : vset_degree) (vertex_by_cost : vset_cost)
    (vertex_by_color : vset_color) (curr_spilled_regs : reg_set) (max_reg : int)
    (cost_map : cost_map) : vset_color * reg_set =
  let next = VSetDegree.min_elt_opt vertex_by_degree in
  match next with
  | Some vertex ->
      if vertex.degree < max_reg then (
        VertexStack.push vertex vertex_stack;
        let new_vertex_by_degree, new_vertex_by_cost, new_vertex_by_color =
          implicit_remove_vertex int_graph vertex vertex_by_degree
            vertex_by_cost vertex_by_color
        in
        push_by_degree int_graph vertex_stack new_vertex_by_degree
          new_vertex_by_cost new_vertex_by_color curr_spilled_regs max_reg
          cost_map)
      else
        push_by_cost int_graph vertex_stack vertex_by_degree vertex_by_cost
          vertex_by_color curr_spilled_regs max_reg cost_map
  | None ->
      try_color int_graph vertex_stack vertex_by_degree vertex_by_cost
        vertex_by_color curr_spilled_regs max_reg cost_map

(** Simplification step that removes the cheapest vertex *)
and push_by_cost (int_graph : interference_graph) (vertex_stack : vstack)
    (vertex_by_degree : vset_degree) (vertex_by_cost : vset_cost)
    (vertex_by_color : vset_color) (curr_spilled_regs : reg_set) (max_reg : int)
    (cost_map : cost_map) : vset_color * reg_set =
  let next = VSetCost.min_elt_opt vertex_by_cost in
  match next with
  | None -> raise (Failure "No vertex available in vertex_by_cost")
  | Some vertex ->
      VertexStack.push vertex vertex_stack;
      let new_vertex_by_degree, new_vertex_by_cost, new_vertex_by_color =
        implicit_remove_vertex int_graph vertex vertex_by_degree vertex_by_cost
          vertex_by_color
      in
      push_by_degree int_graph vertex_stack new_vertex_by_degree
        new_vertex_by_cost new_vertex_by_color curr_spilled_regs max_reg
        cost_map

(** Color the simplified graph or trigger a spill *)
and try_color (int_graph : interference_graph) (vertex_stack : vstack)
    (vertex_by_degree : vset_degree) (vertex_by_cost : vset_cost)
    (vertex_by_color : vset_color) (curr_spilled_regs : reg_set) (max_reg : int)
    (cost_map : cost_map) : vset_color * reg_set =
  match VertexStack.is_empty vertex_stack with
  | true -> (vertex_by_color, curr_spilled_regs)
  | false -> (
      let next = VertexStack.pop vertex_stack in
      let neighbours = RMap.find next.reg int_graph in
      let colored_neighbors =
        VSetColor.filter (fun v -> RSet.mem v.reg neighbours) vertex_by_color
      in
      let max_neighbour_color = VSetColor.max_elt_opt colored_neighbors in
      match max_neighbour_color with
      | None ->
          let node = { next with color = 0 } in
          let new_vertex_by_color = VSetColor.add node vertex_by_color in
          try_color int_graph vertex_stack vertex_by_degree vertex_by_cost
            new_vertex_by_color curr_spilled_regs max_reg cost_map
      | Some max_color when max_color.color < max_reg - 1 ->
          let node = { next with color = max_color.color + 1 } in
          let new_vertex_by_color = VSetColor.add node vertex_by_color in
          try_color int_graph vertex_stack vertex_by_degree vertex_by_cost
            new_vertex_by_color curr_spilled_regs max_reg cost_map
      | Some max_color ->
          let new_spilled_regs = RSet.add next.reg curr_spilled_regs in
          (* spill this vertex and rebuild the graph without it *)
          let ( new_int_graph,
                new_vertex_by_degree,
                new_vertex_by_cost,
                new_vertex_by_color ) =
            init_interference_graph int_graph (Some next.reg) cost_map
          in
          push_by_degree new_int_graph (VertexStack.create ())
            new_vertex_by_degree new_vertex_by_cost new_vertex_by_color
            new_spilled_regs max_reg cost_map)

(** Build the final register coloring map from colored vertices *)
let build_color_map (colors : vset_color) : color_map =
  let initial_color_map =
    VSetColor.fold
      (fun vertex acc ->
        if vertex.color < 0 then
          raise (Failure "Invalid color produced by register allocator")
        else if vertex.color = 0 then RMap.add vertex.reg Rin acc
        else if vertex.color = 1 then RMap.add vertex.reg Rout acc
        else RMap.add vertex.reg (RVar (vertex.color - 2)) acc)
      colors RMap.empty
  in
  RMap.add Rb Rb (RMap.add Ra Ra initial_color_map)

let find_color (color_map : color_map) (reg : reg) : reg =
  match reg with
  | Ra | Rb -> reg
  | _ -> (
    match RMap.find_opt reg color_map with
    | Some c -> c
    | None ->
        raise
          (Failure
              "Register allocator produced an unmapped \
              register"))

(** Apply the color map to every instruction in the CFG *)
let color_cfg (risc_cfg : risc_cfg) (color_map : color_map)
    (spilled_regs : reg_set) : risc_cfg =
  let find_color = find_color color_map in
  let colored_nodes =
    IMap.map
      (fun instrs ->
        let colored_instructions =
          List.map
            (fun instr ->
              match instr with
              | Nop | Jump _ -> instr
              | Brop (op, r1, r2, r3) ->
                  Brop (op, find_color r1, find_color r2, find_color r3)
              | Biop (op, r1, n, r2) ->
                  Biop (op, find_color r1, n, find_color r2)
              | Urop (op, r1, r2) -> Urop (op, find_color r1, find_color r2)
              | Load (r1, addr) -> Load (find_color r1, addr)
              | LoadI (n, r1) -> LoadI (n, find_color r1)
              | Store (r1, r2) -> Store (find_color r1, find_color r2)
              | CJump (r, l1, l2) -> CJump (find_color r, l1, l2))
            instrs
        in
        colored_instructions)
      risc_cfg.nodes
  in
  { risc_cfg with nodes = colored_nodes }

(** Compute the memory address associated to a register *)
let spill_slot_of_reg (reg : reg) : int =
  match reg with
  | Rin -> 0
  | Rout -> 1
  | RVar id -> id + 2
  | Ra | Rb ->
      raise
        (Error
           "Cannot assign a spill slot to reserved temporary registers Ra/Rb")

(** Build the mapping from spilled registers to their memory address *)
let build_address_map (spilled_regs : reg_set) : address_map =
  RSet.fold
    (fun reg acc ->
      let slot = spill_slot_of_reg reg in
      RMap.add reg slot acc)
    spilled_regs RMap.empty

(** Emit loads for spilled operands prior to an instruction *)
let load_used (temp_reg : reg) (address_map : address_map)
    (spilled_regs : reg_set) (reg : reg) : instruction list * reg =
  if RSet.mem reg spilled_regs then
    let addr = RMap.find reg address_map in
    ([ LoadI (addr, temp_reg); Load (temp_reg, temp_reg) ], temp_reg)
  else ([], reg)

(** Emit stores for spilled results after an instruction *)
let store_defined (address_map : address_map) (spilled_regs : reg_set)
    (reg : reg) : instruction list * reg =
  if RSet.mem reg spilled_regs then
    let addr = RMap.find reg address_map in
    ([ LoadI (addr, Ra); Store (Rb, Ra) ], Rb)
  else ([], reg)

(** Rewrite a single instruction to insert loads/stores for spilled registers *)
let rewrite_instruction_for_spill (address_map : address_map)
    (spilled_regs : reg_set) (instr : instruction) =
  match instr with
  | Nop | Jump _ -> [ instr ]
  | LoadI (n, r) ->
      let store, target_reg = store_defined address_map spilled_regs r in
      LoadI (n, target_reg) :: store
  | Brop (op, r1, r2, r3) ->
      let load1, op1 = load_used Ra address_map spilled_regs r1 in
      let load2, op2 =
        if r2 <> r1 then load_used Rb address_map spilled_regs r2 else ([], op1)
      in
      let store, r3 = store_defined address_map spilled_regs r3 in
      (* loads happen before the op, store after it *)
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
      let load2, op2 =
        if r2 <> r1 then load_used Rb address_map spilled_regs r2 else ([], op1)
      in
      load1 @ load2 @ [ Store (op1, op2) ]
  | CJump (r, l1, l2) ->
      let load1, op1 = load_used Ra address_map spilled_regs r in
      load1 @ [ CJump (op1, l1, l2) ]

(** Rewrite the whole RISC CFG to materialize spilled registers *)
let spill_cfg (risc_cfg : risc_cfg) (spilled_regs : reg_set) : risc_cfg =
  if RSet.is_empty spilled_regs then risc_cfg
  else
    let address_map = build_address_map spilled_regs in
    let spilled_nodes =
      IMap.map
        (fun instrs ->
          let spilled_instructions =
            List.concat
              (List.map
                 (rewrite_instruction_for_spill address_map spilled_regs)
                 instrs)
          in
          spilled_instructions)
        risc_cfg.nodes
    in
    { risc_cfg with nodes = spilled_nodes }

(** Insert prologue/epilogue to preserve Rin/Rout *)
let add_prologue_epilogue (risc_cfg : risc_cfg) (spilled_regs : reg_set)
    (color_map : color_map) : risc_cfg =
  let address_map = build_address_map spilled_regs in
  let initial_id = risc_cfg.initial in
  let nodes = risc_cfg.nodes in
  let nodes =
    let init_instrs = IMap.find initial_id nodes in
    let prologue =
      if RSet.mem Rin spilled_regs then
        let slot = RMap.find Rin address_map in
        [ LoadI (slot, Ra); Store (Rin, Ra) ]
      else
        match RMap.find_opt Rin color_map with
        | Some r when r <> Rin -> [ Urop (Copy, Rin, r) ]
        | _ -> []
    in
    IMap.add initial_id (prologue @ init_instrs) nodes
  in
  let nodes =
    List.fold_left
      (fun acc final_id ->
        let instrs = IMap.find final_id acc in
        let epilogue =
          if RSet.mem Rout spilled_regs then
            let slot = RMap.find Rout address_map in
            [ LoadI (slot, Ra); Load (Ra, Rout) ]
          else
            match RMap.find_opt Rout color_map with
            | Some r when r <> Rout -> [ Urop (Copy, r, Rout) ]
            | _ -> []
        in
        IMap.add final_id (instrs @ epilogue) acc)
      nodes risc_cfg.final
  in
  { risc_cfg with nodes }

(** Perform the full global register allocation pipeline *)
let global_allocation (risc_cfg : risc_cfg) (df_risc_cfg : dataflow_risc_cfg)
    (max_reg : int) (var_to_reg : var_to_reg) (use_live : bool) =
  let usable_regs = max_reg - 2 in
  let base_graph, extend_reg_map =
    initialize_interference_graph risc_cfg var_to_reg
  in
  let interference_graph =
    if use_live then build_with_liveness base_graph df_risc_cfg
    else build_without_liveness base_graph extend_reg_map
  in
  let cost_map = compute_cost_map risc_cfg in
  let interference_graph, vertex_by_degree, vertex_by_cost, vertex_by_color =
    init_interference_graph interference_graph None cost_map
  in
  let colors, spilled_regs =
    push_by_degree interference_graph (VertexStack.create ()) vertex_by_degree
      vertex_by_cost vertex_by_color RSet.empty usable_regs cost_map
  in
  let color_map = build_color_map colors in
  let spilled_cfg = spill_cfg risc_cfg spilled_regs in
  let colored_and_spilled_cfg = color_cfg spilled_cfg color_map spilled_regs in
  let final_cfg =
    add_prologue_epilogue colored_and_spilled_cfg spilled_regs color_map
  in
  (final_cfg, color_map, spilled_regs)
