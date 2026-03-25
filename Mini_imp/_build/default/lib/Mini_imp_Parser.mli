
(* The type of tokens. *)

type token = 
  | WITH
  | WHILE
  | VAR of (string)
  | TIMES
  | THEN
  | SKIP
  | RPAREN
  | PLUS
  | OUTPUT
  | OR
  | OF_BOOL
  | NOT
  | MINUS
  | MINOR
  | MAIN
  | LPAREN
  | INT of (int)
  | INPUT
  | IF
  | EOF
  | ELSE
  | DO
  | DEF
  | CONCAT
  | BOOL of (bool)
  | ASSIGN
  | AS
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prg: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Mini_imp_AST.program)
