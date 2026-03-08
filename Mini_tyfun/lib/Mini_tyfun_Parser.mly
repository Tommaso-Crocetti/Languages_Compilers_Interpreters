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
%token INT_TYPE BOOL_TYPE ARROW
%token EOF


%start <term> prg

%% 

prg: 
    t = t ; EOF { t }
t: 
  | n = INT { int_ n }
  | v = BOOL { bool_ v }
  | x = VAR { var x }
  | LET ; x = VAR ; EQUAL ; t1 = t ; IN ; t2 = t { let_ x t1 t2 }
  | LETFUN ; f = VAR ; x = VAR ; COLON ; t1 = fun_type ; EQUAL ; t2 = t ; IN ; t3 = t { letfun f x t1 t2 t3 }
  | t1 = t ; t2 = t { app t1 t2 }
  | t1 = t ; PLUS ; t2 = t { op plus t1 t2 }
  | t1 = t ; MINUS ; t2 = t { op minus t1 t2 }
  | t1 = t ; TIMES ; t2 = t { op times t1 t2 }
  | NOT ; t = t { not_ t }
  | t1 = t ; AND ; t2 = t { op and_ t1 t2 }
  | t1 = t ; OR ; t2 = t { op or_ t1 t2 }
  | t1 = t ; MINOR ; t2 = t { op minor t1 t2 }
  | IF ; t1 = t ; THEN ; t2 = t ; ELSE ; t3 = t { if_ t1 t2 t3 }
  | FUN ; x = VAR ; COLON ; t1 = fun_type ; FUN_ARROW ; t2 = t { fun_ x t1 t2 }
  | LPAREN ; t = t ; RPAREN ; { t }
fun_type:
  | INT_TYPE { int_type }
  | BOOL_TYPE { bool_type }
  | t1 = fun_type ; ARROW ; t2 = fun_type {arrow t1 t2 }
  | LPAREN ; t = fun_type ; RPAREN ; { t }