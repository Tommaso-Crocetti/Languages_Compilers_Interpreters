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

%left OR
%left AND
%left MINOR

%left PLUS MINUS
%left TIMES
%left NOT

%left app

%%

prg:
  | t = expr ; EOF { t }

expr:
  | LET ; x = VAR ; EQUAL ; t1 = expr ; IN ; t2 = expr { let_ x t1 t2 }
  | LETFUN ; f = VAR ; x = VAR ; COLON ; ty = fun_type ; EQUAL ; body = expr ; IN ; cont = expr { letfun f x ty body cont }
  | IF ; c = expr ; THEN ; t = expr ; ELSE ; e = expr { if_ c t e }
  | FUN ; x = VAR ; COLON ; ty = fun_type ; FUN_ARROW ; body = expr { fun_ x ty body }
  | t = op_expr { t }

op_expr:
  | t1 = op_expr ; OR ; t2 = op_expr { op or_ t1 t2 }
  | t1 = op_expr ; AND ; t2 = op_expr { op and_ t1 t2 }
  | t1 = op_expr ; MINOR ; t2 = op_expr { op minor t1 t2 }
  | t1 = op_expr ; PLUS ; t2 = op_expr { op plus t1 t2 }
  | t1 = op_expr ; MINUS ; t2 = op_expr { op minus t1 t2 }
  | t1 = op_expr ; TIMES ; t2 = op_expr { op times t1 t2 }
  | NOT ; t = op_expr { not_ t }
  | t = app_expr %prec app { t }

app_expr:
  | t1 = app_expr ; t2 = atom { app t1 t2 }
  | t = atom { t }

atom:
  | n = int { int_ n }
  | v = BOOL { bool_ v }
  | x = VAR { var x }
  | LPAREN ; t = expr ; RPAREN { t }

int:
  | i = INT { i }
  | MINUS ; i = int { -i }

fun_type:
  | t1 = fun_type_atom ; ARROW_TYPE ; t2 = fun_type { arrow t1 t2 }
  | t = fun_type_atom { t }

fun_type_atom:
  | INT_TYPE { int_type }
  | BOOL_TYPE { bool_type }
  | LPAREN ; t = fun_type ; RPAREN { t }