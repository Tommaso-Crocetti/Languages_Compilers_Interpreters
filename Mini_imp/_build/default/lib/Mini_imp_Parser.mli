
(* The type of tokens. *)

type token = 
  | WHILE
  | VAR of (string)
  | TRUE
  | TIMES
  | THEN
  | SKIP
  | RPAREN
  | PROGRAM of (string * string)
  | PLUS
  | OR
  | OF_BOOL
  | NOT
  | MINUS
  | MINOR
  | LPAREN
  | INT of (int)
  | IF
  | FALSE
  | EOF
  | ELSE
  | DO
  | CONCAT
  | ASSIGN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prg: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Mini_imp.program)
