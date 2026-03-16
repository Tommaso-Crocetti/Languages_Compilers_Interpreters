%{
  open Mini_imp_Interpreter
%}

%token <int> INT
%token <bool> BOOL
%token <string> VAR
%token DEF MAIN WITH INPUT OUTPUT AS
%token PLUS MINUS TIMES OF_BOOL
%token AND OR NOT MINOR
%token SKIP
%token ASSIGN
%token CONCAT
%token IF THEN ELSE
%token WHILE DO
%token LPAREN RPAREN
%token EOF


%start <program> prg

%right CONCAT
%nonassoc ELSE DO

%left OR
%left AND
%nonassoc NOT

%nonassoc OF_BOOL
%nonassoc MINOR

%left PLUS MINUS
%left TIMES

%%

prg:
    | DEF MAIN WITH INPUT ; x = VAR ; OUTPUT ; y = VAR ; AS ; body = cmd ; EOF
      { make_program x y body }
cmd:
    | SKIP { skip }
    | x = VAR ; ASSIGN ; a = a_exp { assign x a }
    | c1 = cmd ; CONCAT ; c2 = cmd { seq c1 c2 }
    | IF ; b = b_exp ; THEN ; c1 = cmd ; ELSE ; c2 = cmd { if_ b c1 c2 }
    | WHILE ; b = b_exp ; DO ; c = cmd { while_ b c }
    | LPAREN ; c = cmd ; RPAREN ; { c }
a_exp:
    | n = int { aval n }
    | x = VAR { var x }
    | a1 = a_exp ; PLUS ; a2 = a_exp { plus a1 a2 }
    | a1 = a_exp ; MINUS ; a2 = a_exp { minus a1 a2 }
    | a1 = a_exp ; TIMES ; a2 = a_exp { times a1 a2 }
    | OF_BOOL ; b = b_exp { of_bool b }
    | LPAREN ; a = a_exp ; RPAREN { a }
int:
    | i = INT { i }
    | MINUS ; i = INT { -i }
b_exp:
    | v = BOOL { bval v }
    | b1 = b_exp ; AND ; b2 = b_exp { and_ b1 b2 }
    | b1 = b_exp ; OR ; b2 = b_exp { or_ b1 b2 }
    | NOT ; b = b_exp { not_ b }
    | a1 = a_exp ; MINOR ; a2 = a_exp { minor a1 a2 }
    | LPAREN ; b = b_exp ; RPAREN ; { b }