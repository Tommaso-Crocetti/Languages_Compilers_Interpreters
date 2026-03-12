{
  open Mini_tyfun_Parser
  exception Error of string
}

let int = ['0'-'9']['0'-'9']*
let bool = "true" | "false"
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let white = [' ' '\t' '\n' '\r']+ | "\r\n"

rule read = parse

  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { TIMES }

  | "and"     { AND }
  | "or"      { OR }
  | "not"     { NOT }
  | "<"  { MINOR }

  | "let"    { LET }
  | "="      { EQUAL }
  | "in"     { IN }

  | "letfun" { LETFUN }
  | ":" { COLON }

  | "fun"   { FUN }
  | "=>"   { FUN_ARROW }
  
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }

  | "("  { LPAREN }
  | ")"  { RPAREN }

  | "int" { INT_TYPE }
  | "bool" { BOOL_TYPE }
  | "->" { ARROW_TYPE }

  | eof  { EOF }

  | int as n  { INT (int_of_string n) }
  | bool as b { BOOL (b = "true") }
  | id as x   { VAR x }
  | white   { read lexbuf }

  | _ { raise (Error ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }