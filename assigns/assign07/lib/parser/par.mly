%{
open Utils

let rec mk_app e es =
  match es with
  | [] -> e
  | x :: es -> mk_app (App (e, x)) es
%}

%token <int> NUM
%token <string> VAR
%token EOF

(*let*)
%token LET
%token EQUALS
%token IN 

(*if*)
%token IF
%token THEN
%token ELSE

(*Fun*)
%token FUN

(*Arrow*)
%token ARROW

(*BOP*)
%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token LT
%token LTE
%token GT
%token GTE
%token NEQ
%token AND
%token OR


%right OR
%right AND
%left LT 
%left LTE 
%left GT 
%left GTE 
%left NEQ
%left ADD 
%left SUB
%left MUL 
%left DIV 
%left MOD

(*expr3 tokens*)
%token LPAREN
%token RPAREN
%token TRUE
%token FALSE
%token UNIT



%start <Utils.prog> prog

%%

prog:
  | e = expr; EOF { e }

expr:
  | IF; e = expr; THEN; e1 = expr; ELSE; e2 = expr
    { If (e, e1, e2)}
  | LET; x = VAR; EQUALS; e1 = expr; IN; e2 = expr 
    { Let (x, e1, e2) }
  | FUN; x = VAR; ARROW; e = expr (*This is dif from lecture*)
    { Fun (x, e)}
  | e = expr2 { e }

expr2:
  | e1 = expr2; op = bop; e2 = expr2 
    { Bop (op, e1, e2) }
  | e = expr3; es = expr3_list
    { mk_app e es } (*from lecture*)
  | e = expr3 { e }

expr3_list:
  | e = expr3; es = expr3_list { e :: es }
  | { [] }

expr3:
  | LPAREN; RPAREN { Unit }
  | LPAREN; e = expr; RPAREN { e }
  | n = NUM { Num n }
  | v = VAR { Var v }
  | TRUE { True }
  | FALSE { False }
  | UNIT { Unit }

%inline bop:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div }
  | MOD { Mod }
  | LT  { Lt  }
  | LTE { Lte }
  | GT  { Gt  }
  | GTE { Gte }
  | NEQ { Neq }
  | AND { And }
  | OR  { Or }