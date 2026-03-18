%{
  open Mini_imp_AST
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
      { { input_var = x; output_var = y; body } }
cmd:
    | SKIP { Skip }
    | x = VAR ; ASSIGN ; a = a_exp { Assign (x, a) }
    | c1 = cmd ; CONCAT ; c2 = cmd { Seq (c1, c2) }
    | IF ; b = b_exp ; THEN ; c1 = cmd ; ELSE ; c2 = cmd { If (b, c1, c2) }
    | WHILE ; b = b_exp ; DO ; c = cmd { While (b, c) }
    | LPAREN ; c = cmd ; RPAREN ; { c }
a_exp:
    | n = int { Aval n }
    | x = VAR { Var x }
    | a1 = a_exp ; PLUS ; a2 = a_exp { Plus (a1, a2) }
    | a1 = a_exp ; MINUS ; a2 = a_exp { Minus (a1, a2) }
    | a1 = a_exp ; TIMES ; a2 = a_exp { Times (a1, a2) }
    | OF_BOOL ; b = b_exp { Of_Bool b }
    | LPAREN ; a = a_exp ; RPAREN { a }
int:
    | i = INT { i }
    | MINUS ; i = INT { -i }
b_exp:
    | v = BOOL { Bval v }
    | b1 = b_exp ; AND ; b2 = b_exp { And (b1, b2) }
    | b1 = b_exp ; OR ; b2 = b_exp { Or (b1, b2) }
    | NOT ; b = b_exp { Not b }
    | a1 = a_exp ; MINOR ; a2 = a_exp { Minor (a1, a2) }
    | LPAREN ; b = b_exp ; RPAREN ; { b }