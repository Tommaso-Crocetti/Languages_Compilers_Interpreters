
(* The type of tokens. *)

type token = 
  | VAR of (string)
  | TIMES
  | THEN
  | RPAREN
  | PLUS
  | OR
  | NOT
  | MINUS
  | MINOR
  | LPAREN
  | LETFUN
  | LET
  | INT_TYPE
  | INT of (int)
  | IN
  | IF
  | FUN_ARROW
  | FUN
  | EQUAL
  | EOF
  | ELSE
  | COLON
  | BOOL_TYPE
  | BOOL of (bool)
  | ARROW_TYPE
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prg: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Mini_tyfun.term)
