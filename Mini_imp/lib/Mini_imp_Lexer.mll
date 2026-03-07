{
  open Mini_imp_Parser
  exception Error of string
}

let int = ['0'-'9']['0'-'9']*
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let program = "def main with input " (id as x) " output " (id as y) " as"
let white = [' ' '\t' '\n' '\r']+ | "\r\n"

rule read = parse

  | "true"    { TRUE }
  | "false"   { FALSE }

  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { TIMES }
  | "of bool" { OF_BOOL }

  | "and"     { AND }
  | "or"      { OR }
  | "not"     { NOT }
  | "<"  { MINOR }

  | "skip"    { SKIP }

  | ":=" { ASSIGN }

  | ";"  { CONCAT }

  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }

  | "while"   { WHILE }
  | "do"      { DO }

  | "("  { LPAREN }
  | ")"  { RPAREN }

  | eof  { EOF }

  | int as n  { INT (int_of_string n) }
  | id as x   { VAR x }
  | program { PROGRAM (x, y) }
  | white   { read lexbuf }

  | _ { raise (Error ("Unexpected character: " ^ Lexing.lexeme lexbuf)) }