
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | WHILE
    | VAR of 
# 6 "lib/Mini_imp_Parser.mly"
       (string)
# 16 "lib/Mini_imp_Parser.ml"
  
    | TRUE
    | TIMES
    | THEN
    | SKIP
    | RPAREN
    | PROGRAM of 
# 7 "lib/Mini_imp_Parser.mly"
       (string * string)
# 26 "lib/Mini_imp_Parser.ml"
  
    | PLUS
    | OR
    | OF_BOOL
    | NOT
    | MINUS
    | MINOR
    | LPAREN
    | INT of 
# 5 "lib/Mini_imp_Parser.mly"
       (int)
# 38 "lib/Mini_imp_Parser.ml"
  
    | IF
    | FALSE
    | EOF
    | ELSE
    | DO
    | CONCAT
    | ASSIGN
    | AND
  
end

include MenhirBasics

# 1 "lib/Mini_imp_Parser.mly"
  
  open Mini_imp

# 57 "lib/Mini_imp_Parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState01 : ('s _menhir_cell0_PROGRAM, _menhir_box_prg) _menhir_state
    (** State 01.
        Stack shape : PROGRAM.
        Start symbol: prg. *)

  | MenhirState02 : (('s, _menhir_box_prg) _menhir_cell1_WHILE, _menhir_box_prg) _menhir_state
    (** State 02.
        Stack shape : WHILE.
        Start symbol: prg. *)

  | MenhirState05 : (('s, _menhir_box_prg) _menhir_cell1_OF_BOOL, _menhir_box_prg) _menhir_state
    (** State 05.
        Stack shape : OF_BOOL.
        Start symbol: prg. *)

  | MenhirState06 : (('s, _menhir_box_prg) _menhir_cell1_NOT, _menhir_box_prg) _menhir_state
    (** State 06.
        Stack shape : NOT.
        Start symbol: prg. *)

  | MenhirState10 : (('s, _menhir_box_prg) _menhir_cell1_b_exp, _menhir_box_prg) _menhir_state
    (** State 10.
        Stack shape : b_exp.
        Start symbol: prg. *)

  | MenhirState12 : (('s, _menhir_box_prg) _menhir_cell1_b_exp, _menhir_box_prg) _menhir_state
    (** State 12.
        Stack shape : b_exp.
        Start symbol: prg. *)

  | MenhirState15 : (('s, _menhir_box_prg) _menhir_cell1_a_exp, _menhir_box_prg) _menhir_state
    (** State 15.
        Stack shape : a_exp.
        Start symbol: prg. *)

  | MenhirState17 : (('s, _menhir_box_prg) _menhir_cell1_a_exp, _menhir_box_prg) _menhir_state
    (** State 17.
        Stack shape : a_exp.
        Start symbol: prg. *)

  | MenhirState19 : (('s, _menhir_box_prg) _menhir_cell1_a_exp, _menhir_box_prg) _menhir_state
    (** State 19.
        Stack shape : a_exp.
        Start symbol: prg. *)

  | MenhirState21 : (('s, _menhir_box_prg) _menhir_cell1_a_exp, _menhir_box_prg) _menhir_state
    (** State 21.
        Stack shape : a_exp.
        Start symbol: prg. *)

  | MenhirState25 : ((('s, _menhir_box_prg) _menhir_cell1_WHILE, _menhir_box_prg) _menhir_cell1_b_exp, _menhir_box_prg) _menhir_state
    (** State 25.
        Stack shape : WHILE b_exp.
        Start symbol: prg. *)

  | MenhirState27 : (('s, _menhir_box_prg) _menhir_cell1_VAR, _menhir_box_prg) _menhir_state
    (** State 27.
        Stack shape : VAR.
        Start symbol: prg. *)

  | MenhirState30 : (('s, _menhir_box_prg) _menhir_cell1_IF, _menhir_box_prg) _menhir_state
    (** State 30.
        Stack shape : IF.
        Start symbol: prg. *)

  | MenhirState32 : ((('s, _menhir_box_prg) _menhir_cell1_IF, _menhir_box_prg) _menhir_cell1_b_exp, _menhir_box_prg) _menhir_state
    (** State 32.
        Stack shape : IF b_exp.
        Start symbol: prg. *)

  | MenhirState34 : (((('s, _menhir_box_prg) _menhir_cell1_IF, _menhir_box_prg) _menhir_cell1_b_exp, _menhir_box_prg) _menhir_cell1_cmd, _menhir_box_prg) _menhir_state
    (** State 34.
        Stack shape : IF b_exp cmd.
        Start symbol: prg. *)

  | MenhirState36 : (('s, _menhir_box_prg) _menhir_cell1_cmd, _menhir_box_prg) _menhir_state
    (** State 36.
        Stack shape : cmd.
        Start symbol: prg. *)


and ('s, 'r) _menhir_cell1_a_exp = 
  | MenhirCell1_a_exp of 's * ('s, 'r) _menhir_state * (Mini_imp.a_exp)

and ('s, 'r) _menhir_cell1_b_exp = 
  | MenhirCell1_b_exp of 's * ('s, 'r) _menhir_state * (Mini_imp.b_exp)

and ('s, 'r) _menhir_cell1_cmd = 
  | MenhirCell1_cmd of 's * ('s, 'r) _menhir_state * (Mini_imp.command)

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_NOT = 
  | MenhirCell1_NOT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_OF_BOOL = 
  | MenhirCell1_OF_BOOL of 's * ('s, 'r) _menhir_state

and 's _menhir_cell0_PROGRAM = 
  | MenhirCell0_PROGRAM of 's * 
# 7 "lib/Mini_imp_Parser.mly"
       (string * string)
# 163 "lib/Mini_imp_Parser.ml"


and ('s, 'r) _menhir_cell1_VAR = 
  | MenhirCell1_VAR of 's * ('s, 'r) _menhir_state * 
# 6 "lib/Mini_imp_Parser.mly"
       (string)
# 170 "lib/Mini_imp_Parser.ml"


and ('s, 'r) _menhir_cell1_WHILE = 
  | MenhirCell1_WHILE of 's * ('s, 'r) _menhir_state

and _menhir_box_prg = 
  | MenhirBox_prg of (Mini_imp.program) [@@unboxed]

let _menhir_action_01 =
  fun i ->
    (
# 33 "lib/Mini_imp_Parser.mly"
              ( aval i )
# 184 "lib/Mini_imp_Parser.ml"
     : (Mini_imp.a_exp))

let _menhir_action_02 =
  fun x ->
    (
# 34 "lib/Mini_imp_Parser.mly"
              ( var x )
# 192 "lib/Mini_imp_Parser.ml"
     : (Mini_imp.a_exp))

let _menhir_action_03 =
  fun a1 a2 ->
    (
# 35 "lib/Mini_imp_Parser.mly"
                                     ( plus a1 a2 )
# 200 "lib/Mini_imp_Parser.ml"
     : (Mini_imp.a_exp))

let _menhir_action_04 =
  fun a1 a2 ->
    (
# 36 "lib/Mini_imp_Parser.mly"
                                      ( minus a1 a2 )
# 208 "lib/Mini_imp_Parser.ml"
     : (Mini_imp.a_exp))

let _menhir_action_05 =
  fun a1 a2 ->
    (
# 37 "lib/Mini_imp_Parser.mly"
                                      ( times a1 a2 )
# 216 "lib/Mini_imp_Parser.ml"
     : (Mini_imp.a_exp))

let _menhir_action_06 =
  fun b ->
    (
# 38 "lib/Mini_imp_Parser.mly"
                          ( of_bool b )
# 224 "lib/Mini_imp_Parser.ml"
     : (Mini_imp.a_exp))

let _menhir_action_07 =
  fun () ->
    (
# 40 "lib/Mini_imp_Parser.mly"
           ( bval true )
# 232 "lib/Mini_imp_Parser.ml"
     : (Mini_imp.b_exp))

let _menhir_action_08 =
  fun () ->
    (
# 41 "lib/Mini_imp_Parser.mly"
            ( bval false )
# 240 "lib/Mini_imp_Parser.ml"
     : (Mini_imp.b_exp))

let _menhir_action_09 =
  fun b1 b2 ->
    (
# 42 "lib/Mini_imp_Parser.mly"
                                    ( and_ b1 b2 )
# 248 "lib/Mini_imp_Parser.ml"
     : (Mini_imp.b_exp))

let _menhir_action_10 =
  fun b1 b2 ->
    (
# 43 "lib/Mini_imp_Parser.mly"
                                   ( or_ b1 b2 )
# 256 "lib/Mini_imp_Parser.ml"
     : (Mini_imp.b_exp))

let _menhir_action_11 =
  fun b ->
    (
# 44 "lib/Mini_imp_Parser.mly"
                      ( not_ b )
# 264 "lib/Mini_imp_Parser.ml"
     : (Mini_imp.b_exp))

let _menhir_action_12 =
  fun a1 a2 ->
    (
# 45 "lib/Mini_imp_Parser.mly"
                                      ( minor a1 a2 )
# 272 "lib/Mini_imp_Parser.ml"
     : (Mini_imp.b_exp))

let _menhir_action_13 =
  fun () ->
    (
# 27 "lib/Mini_imp_Parser.mly"
           ( skip )
# 280 "lib/Mini_imp_Parser.ml"
     : (Mini_imp.command))

let _menhir_action_14 =
  fun a x ->
    (
# 28 "lib/Mini_imp_Parser.mly"
                                   ( assign x a )
# 288 "lib/Mini_imp_Parser.ml"
     : (Mini_imp.command))

let _menhir_action_15 =
  fun c1 c2 ->
    (
# 29 "lib/Mini_imp_Parser.mly"
                                   ( seq c1 c2 )
# 296 "lib/Mini_imp_Parser.ml"
     : (Mini_imp.command))

let _menhir_action_16 =
  fun b c1 c2 ->
    (
# 30 "lib/Mini_imp_Parser.mly"
                                                         ( if_ b c1 c2 )
# 304 "lib/Mini_imp_Parser.ml"
     : (Mini_imp.command))

let _menhir_action_17 =
  fun b c ->
    (
# 31 "lib/Mini_imp_Parser.mly"
                                       ( while_ b c )
# 312 "lib/Mini_imp_Parser.ml"
     : (Mini_imp.command))

let _menhir_action_18 =
  fun body t ->
    (
# 25 "lib/Mini_imp_Parser.mly"
                                     ( make_program (fst t) (snd t) body )
# 320 "lib/Mini_imp_Parser.ml"
     : (Mini_imp.program))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | WHILE ->
        "WHILE"
    | VAR _ ->
        "VAR"
    | TRUE ->
        "TRUE"
    | TIMES ->
        "TIMES"
    | THEN ->
        "THEN"
    | SKIP ->
        "SKIP"
    | RPAREN ->
        "RPAREN"
    | PROGRAM _ ->
        "PROGRAM"
    | PLUS ->
        "PLUS"
    | OR ->
        "OR"
    | OF_BOOL ->
        "OF_BOOL"
    | NOT ->
        "NOT"
    | MINUS ->
        "MINUS"
    | MINOR ->
        "MINOR"
    | LPAREN ->
        "LPAREN"
    | INT _ ->
        "INT"
    | IF ->
        "IF"
    | FALSE ->
        "FALSE"
    | EOF ->
        "EOF"
    | ELSE ->
        "ELSE"
    | DO ->
        "DO"
    | CONCAT ->
        "CONCAT"
    | ASSIGN ->
        "ASSIGN"
    | AND ->
        "AND"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let rec _menhir_run_02 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prg) _menhir_state -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_WHILE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState02 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OF_BOOL ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_03 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prg) _menhir_state -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let x = _v in
      let _v = _menhir_action_02 x in
      _menhir_goto_a_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_a_exp : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prg) _menhir_state -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState02 ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState05 ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState06 ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState10 ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState12 ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState30 ->
          _menhir_run_14 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState15 ->
          _menhir_run_16 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState17 ->
          _menhir_run_18 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState19 ->
          _menhir_run_20 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState21 ->
          _menhir_run_22 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState27 ->
          _menhir_run_28 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_14 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prg) _menhir_state -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_a_exp (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINOR ->
          let _menhir_s = MenhirState21 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | OF_BOOL ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_15 : type  ttv_stack. (ttv_stack, _menhir_box_prg) _menhir_cell1_a_exp -> _ -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState15 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | OF_BOOL ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_05 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prg) _menhir_state -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_OF_BOOL (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState05 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OF_BOOL ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_04 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prg) _menhir_state -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_07 () in
      _menhir_goto_b_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_b_exp : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prg) _menhir_state -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState06 ->
          _menhir_run_09 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState10 ->
          _menhir_run_11 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState12 ->
          _menhir_run_13 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState05 ->
          _menhir_run_23 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState02 ->
          _menhir_run_24 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState30 ->
          _menhir_run_31 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_09 : type  ttv_stack. ((ttv_stack, _menhir_box_prg) _menhir_cell1_NOT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prg) _menhir_state -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OR ->
          let _menhir_stack = MenhirCell1_b_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_b_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONCAT | DO | ELSE | EOF | MINOR | MINUS | PLUS | THEN | TIMES ->
          let MenhirCell1_NOT (_menhir_stack, _menhir_s) = _menhir_stack in
          let b = _v in
          let _v = _menhir_action_11 b in
          _menhir_goto_b_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_10 : type  ttv_stack. (ttv_stack, _menhir_box_prg) _menhir_cell1_b_exp -> _ -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState10 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OF_BOOL ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_06 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prg) _menhir_state -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_NOT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState06 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OF_BOOL ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_07 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prg) _menhir_state -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let i = _v in
      let _v = _menhir_action_01 i in
      _menhir_goto_a_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_08 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prg) _menhir_state -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_08 () in
      _menhir_goto_b_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_12 : type  ttv_stack. (ttv_stack, _menhir_box_prg) _menhir_cell1_b_exp -> _ -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState12 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OF_BOOL ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_11 : type  ttv_stack. ((ttv_stack, _menhir_box_prg) _menhir_cell1_b_exp as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prg) _menhir_state -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OR ->
          let _menhir_stack = MenhirCell1_b_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_b_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONCAT | DO | ELSE | EOF | MINOR | MINUS | PLUS | THEN | TIMES ->
          let MenhirCell1_b_exp (_menhir_stack, _menhir_s, b1) = _menhir_stack in
          let b2 = _v in
          let _v = _menhir_action_10 b1 b2 in
          _menhir_goto_b_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_13 : type  ttv_stack. ((ttv_stack, _menhir_box_prg) _menhir_cell1_b_exp as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prg) _menhir_state -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OR ->
          let _menhir_stack = MenhirCell1_b_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_b_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONCAT | DO | ELSE | EOF | MINOR | MINUS | PLUS | THEN | TIMES ->
          let MenhirCell1_b_exp (_menhir_stack, _menhir_s, b1) = _menhir_stack in
          let b2 = _v in
          let _v = _menhir_action_09 b1 b2 in
          _menhir_goto_b_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_23 : type  ttv_stack. ((ttv_stack, _menhir_box_prg) _menhir_cell1_OF_BOOL as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prg) _menhir_state -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OR ->
          let _menhir_stack = MenhirCell1_b_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          let _menhir_stack = MenhirCell1_b_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONCAT | DO | ELSE | EOF | MINOR | MINUS | PLUS | THEN | TIMES ->
          let MenhirCell1_OF_BOOL (_menhir_stack, _menhir_s) = _menhir_stack in
          let b = _v in
          let _v = _menhir_action_06 b in
          _menhir_goto_a_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_24 : type  ttv_stack. ((ttv_stack, _menhir_box_prg) _menhir_cell1_WHILE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prg) _menhir_state -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_b_exp (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | OR ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DO ->
          let _menhir_s = MenhirState25 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SKIP ->
              _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AND ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_26 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prg) _menhir_state -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_VAR (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | ASSIGN ->
          let _menhir_s = MenhirState27 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VAR _v ->
              _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | OF_BOOL ->
              _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_29 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prg) _menhir_state -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_13 () in
      _menhir_goto_cmd _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_cmd : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_prg) _menhir_state -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState32 ->
          _menhir_run_33 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState34 ->
          _menhir_run_35 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState36 ->
          _menhir_run_37 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState25 ->
          _menhir_run_38 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState01 ->
          _menhir_run_39 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_33 : type  ttv_stack. (((ttv_stack, _menhir_box_prg) _menhir_cell1_IF, _menhir_box_prg) _menhir_cell1_b_exp as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prg) _menhir_state -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_cmd (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | ELSE ->
          let _menhir_s = MenhirState34 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SKIP ->
              _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | CONCAT ->
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_30 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_prg) _menhir_state -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState30 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | TRUE ->
          _menhir_run_04 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | OF_BOOL ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_06 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FALSE ->
          _menhir_run_08 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_36 : type  ttv_stack. (ttv_stack, _menhir_box_prg) _menhir_cell1_cmd -> _ -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState36 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | VAR _v ->
          _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | SKIP ->
          _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | IF ->
          _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_35 : type  ttv_stack. ((((ttv_stack, _menhir_box_prg) _menhir_cell1_IF, _menhir_box_prg) _menhir_cell1_b_exp, _menhir_box_prg) _menhir_cell1_cmd as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prg) _menhir_state -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | CONCAT ->
          let _menhir_stack = MenhirCell1_cmd (_menhir_stack, _menhir_s, _v) in
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE | EOF ->
          let MenhirCell1_cmd (_menhir_stack, _, c1) = _menhir_stack in
          let MenhirCell1_b_exp (_menhir_stack, _, b) = _menhir_stack in
          let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
          let c2 = _v in
          let _v = _menhir_action_16 b c1 c2 in
          _menhir_goto_cmd _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_37 : type  ttv_stack. ((ttv_stack, _menhir_box_prg) _menhir_cell1_cmd as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prg) _menhir_state -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | CONCAT ->
          let _menhir_stack = MenhirCell1_cmd (_menhir_stack, _menhir_s, _v) in
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE | EOF ->
          let MenhirCell1_cmd (_menhir_stack, _menhir_s, c1) = _menhir_stack in
          let c2 = _v in
          let _v = _menhir_action_15 c1 c2 in
          _menhir_goto_cmd _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_38 : type  ttv_stack. (((ttv_stack, _menhir_box_prg) _menhir_cell1_WHILE, _menhir_box_prg) _menhir_cell1_b_exp as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prg) _menhir_state -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | CONCAT ->
          let _menhir_stack = MenhirCell1_cmd (_menhir_stack, _menhir_s, _v) in
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ELSE | EOF ->
          let MenhirCell1_b_exp (_menhir_stack, _, b) = _menhir_stack in
          let MenhirCell1_WHILE (_menhir_stack, _menhir_s) = _menhir_stack in
          let c = _v in
          let _v = _menhir_action_17 b c in
          _menhir_goto_cmd _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_39 : type  ttv_stack. (ttv_stack _menhir_cell0_PROGRAM as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prg) _menhir_state -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | EOF ->
          let MenhirCell0_PROGRAM (_menhir_stack, t) = _menhir_stack in
          let body = _v in
          let _v = _menhir_action_18 body t in
          MenhirBox_prg _v
      | CONCAT ->
          let _menhir_stack = MenhirCell1_cmd (_menhir_stack, _menhir_s, _v) in
          _menhir_run_36 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_31 : type  ttv_stack. ((ttv_stack, _menhir_box_prg) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prg) _menhir_state -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_b_exp (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | THEN ->
          let _menhir_s = MenhirState32 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SKIP ->
              _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | OR ->
          _menhir_run_10 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND ->
          _menhir_run_12 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_17 : type  ttv_stack. (ttv_stack, _menhir_box_prg) _menhir_cell1_a_exp -> _ -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState17 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | OF_BOOL ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_19 : type  ttv_stack. (ttv_stack, _menhir_box_prg) _menhir_cell1_a_exp -> _ -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState19 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VAR _v ->
          _menhir_run_03 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | OF_BOOL ->
          _menhir_run_05 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_07 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_16 : type  ttv_stack. ((ttv_stack, _menhir_box_prg) _menhir_cell1_a_exp as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prg) _menhir_state -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_a_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_a_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_a_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | CONCAT | DO | ELSE | EOF | MINOR | OR | THEN ->
          let MenhirCell1_a_exp (_menhir_stack, _menhir_s, a1) = _menhir_stack in
          let a2 = _v in
          let _v = _menhir_action_05 a1 a2 in
          _menhir_goto_a_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_18 : type  ttv_stack. ((ttv_stack, _menhir_box_prg) _menhir_cell1_a_exp as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prg) _menhir_state -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_a_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_a_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_a_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | CONCAT | DO | ELSE | EOF | MINOR | OR | THEN ->
          let MenhirCell1_a_exp (_menhir_stack, _menhir_s, a1) = _menhir_stack in
          let a2 = _v in
          let _v = _menhir_action_03 a1 a2 in
          _menhir_goto_a_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_20 : type  ttv_stack. ((ttv_stack, _menhir_box_prg) _menhir_cell1_a_exp as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prg) _menhir_state -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_a_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_a_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_a_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | CONCAT | DO | ELSE | EOF | MINOR | OR | THEN ->
          let MenhirCell1_a_exp (_menhir_stack, _menhir_s, a1) = _menhir_stack in
          let a2 = _v in
          let _v = _menhir_action_04 a1 a2 in
          _menhir_goto_a_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_22 : type  ttv_stack. ((ttv_stack, _menhir_box_prg) _menhir_cell1_a_exp as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prg) _menhir_state -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_a_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_a_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_a_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | CONCAT | DO | ELSE | EOF | MINOR | OR | THEN ->
          let MenhirCell1_a_exp (_menhir_stack, _menhir_s, a1) = _menhir_stack in
          let a2 = _v in
          let _v = _menhir_action_12 a1 a2 in
          _menhir_goto_b_exp _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_28 : type  ttv_stack. ((ttv_stack, _menhir_box_prg) _menhir_cell1_VAR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_prg) _menhir_state -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TIMES ->
          let _menhir_stack = MenhirCell1_a_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_15 _menhir_stack _menhir_lexbuf _menhir_lexer
      | PLUS ->
          let _menhir_stack = MenhirCell1_a_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_17 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_a_exp (_menhir_stack, _menhir_s, _v) in
          _menhir_run_19 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CONCAT | ELSE | EOF ->
          let MenhirCell1_VAR (_menhir_stack, _menhir_s, x) = _menhir_stack in
          let a = _v in
          let _v = _menhir_action_14 a x in
          _menhir_goto_cmd _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  let _menhir_run_00 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_prg =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | PROGRAM _v ->
          let _menhir_stack = MenhirCell0_PROGRAM (_menhir_stack, _v) in
          let _menhir_s = MenhirState01 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_02 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | VAR _v ->
              _menhir_run_26 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | SKIP ->
              _menhir_run_29 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | IF ->
              _menhir_run_30 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
end

let prg =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_prg v = _menhir_run_00 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v
