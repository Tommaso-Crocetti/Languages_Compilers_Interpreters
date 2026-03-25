%{
  open Mini_tyfun
%}

%token <int> INT
%token <bool> BOOL
%token <string> VAR
%token PLUS MINUS TIMES
%token AND OR NOT MINOR
%token LET EQUAL IN
%token LETFUN COLON
%token FUN FUN_ARROW
%token IF THEN ELSE
%token LPAREN RPAREN
%token INT_TYPE BOOL_TYPE ARROW_TYPE
%token EOF

%start <term> prg

%left op

%left OR
%left AND
%left MINOR

%left PLUS MINUS
%left TIMES

%left app

%%

prg:
  | t = expr ; EOF { t }

expr:
  | LET ; x = VAR ; EQUAL ; t1 = expr ; IN ; t2 = expr { Let (x, t1, t2) }
  | LETFUN ; f = VAR ; x = VAR ; COLON ; ty = fun_type ; EQUAL ; body = expr ; IN ; cont = expr { LetFun (f, x, ty, body, cont) }
  | IF ; c = expr ; THEN ; t = expr ; ELSE ; e = expr { If (c, t, e) }
  | FUN ; x = VAR ; COLON ; ty = fun_type ; FUN_ARROW ; body = expr { Fun (x, ty, body) }
  | t = op_expr %prec op { t }

op_expr:
  | t1 = op_expr ; OR ; t2 = expr { Op (Or, t1, t2) }
  | t1 = op_expr ; AND ; t2 = expr { Op (And, t1, t2) }
  | t1 = op_expr ; MINOR ; t2 = expr { Op (Minor, t1, t2) }
  | t1 = op_expr ; PLUS ; t2 = expr { Op (Plus, t1, t2) }
  | t1 = op_expr ; MINUS ; t2 = expr { Op (Minus, t1, t2) }
  | t1 = op_expr ; TIMES ; t2 = expr { Op (Times, t1, t2) }
  | NOT ; t = expr { Not t }
  | t = app_expr %prec app { t }

app_expr:
  | t1 = app_expr ; t2 = atom { App (t1, t2) }
  | t = atom { t }

atom:
  | n = int { Int n }
  | v = BOOL { Bool v }
  | x = VAR { Var x }
  | LPAREN ; t = expr ; RPAREN { t }

int:
  | i = INT { i }
  | MINUS ; i = int { -i }

fun_type:
  | t1 = fun_type_atom ; ARROW_TYPE ; t2 = fun_type { ArrowT (t1, t2) }
  | t = fun_type_atom { t }

fun_type_atom:
  | INT_TYPE { IntT }
  | BOOL_TYPE { BoolT }
  | LPAREN ; t = fun_type ; RPAREN { t }
