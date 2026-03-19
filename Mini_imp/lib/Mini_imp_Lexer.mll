{
  open Mini_imp_Parser
  exception Error of string
}

let int = ['0'-'9']['0'-'9']*
let bool = "true" | "false"
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let white = [' ' '\t' '\n' '\r']+ | "\r\n"

rule read = parse

  | "def" { DEF }
  | "main" { MAIN }
  | "with" { WITH }
  | "input" { INPUT }
  | "output" { OUTPUT }
  | "as" { AS }

  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { TIMES }
  | "of_bool" { OF_BOOL }

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

  | int as n { INT (int_of_string n) }
  | bool as b { BOOL (b = "true") }
  | id as x   { VAR x }
  | white   { read lexbuf }

  | _ { raise (Error ("unexpected character " ^ Lexing.lexeme lexbuf)) }