%{
open Utils
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
%token EQ
%token NEQ
%token AND
%token OR

%right OR
%right AND
%left LT LTE GT GTE EQ NEQ
%left ADD SUB
%left MUL DIV MOD

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
  | FUN; x = VAR; ARROW; e = expr 
    { Fun (x, e)}
  | e = expr2 {e}

expr2:
  | e1 = expr2; op = bop; e2 = expr2 
    { Bop  (op, e1, e2)}
  | e1 = expr3; e2 = expr2_tail {e1}

expr2_tail: (*recursion!*)
  | e = expr3; es = expr2_tail { App (e, es) }
  | (*empty to show end of expr2_tail*) { e }

(*expr2:
  | e1 = expr2; op = bop; e2 = expr2 
    { Bop (op, e1, e2) }
  | e1 = expr2; e2 = expr3 
    { App (e1, e2) }
  | e = expr3 { e }
  *)

expr3:
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
  | EQ  { Eq }
  | NEQ { Neq }
  | AND { And }
  | OR  { Or }