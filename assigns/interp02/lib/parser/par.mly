%{
open Utils
%}

%token <int> NUM
%token <string> VAR
%token EOF
%token LET "let"
%token EQUALS "="
%token IN "in"
%token IF "if"
%token THEN "then"
%token ELSE "else"
%token FUN "fun"
%token ARROW "->"
%token ADD "+"
%token SUB "-"
%token MUL "*"
%token DIV "/"
%token MOD "mod"
%token LT "<"
%token LTE "<="
%token GT ">"
%token GTE ">="
%token NEQ "<>"
%token AND "&&"
%token OR "||"
%token LPAREN "("
%token RPAREN ")"
%token TRUE "true"
%token FALSE "false"
%token UNIT "unit"
%token REC "rec"
%token INTTY "int"
%token BOOLTY "bool"
%token ASSERT "assert"
%token COLON ":"

%start <Utils.prog> prog

%%

prog:
  | ls = toplet* EOF { ls }

toplet:
  | "let" x = VAR ":" ty = ty "=" e = expr
    { { is_rec = false; name = x; args = []; ty; value = e } }
  | "let" "rec" f = VAR args = arg_list ":" ty_out = ty "=" e = expr
    { { is_rec = true; name = f; args; ty = ty_out; value = e } }

ty:
  | "int" { IntTy }
  | "bool" { BoolTy }
  | "unit" { UnitTy }
  | t1 = ty "->" t2 = ty { FunTy (t1, t2) }
  | "(" ty = ty ")" { ty } 

expr:
  | "let" x = VAR "=" e1 = expr "in" e2 = expr 
    { SLet { is_rec = false; name = x; args = []; ty = UnitTy; value = e1; body = e2 } }
  | "let" "rec" f = VAR args = arg_list "=" e1 = expr "in" e2 = expr
    { SLet { is_rec = true; name = f; args; ty = UnitTy; value = e1; body = e2 } }
  | "if" e1 = expr "then" e2 = expr "else" e3 = expr 
    { SIf(e1, e2, e3) }
  | "fun" x = VAR ":" ty = ty "->" e = expr
    { SFun { arg = (x, ty); args = []; body = e } }
  | e = expr2 { e }

expr2:
  | e1 = expr2 op = bop e2 = expr2 { SBop(op, e1, e2) }
  | "assert" e = expr3 { SAssert(e) }
  | e = expr3 es = expr3* 
    { List.fold_left (fun e1 e2 -> SApp(e1, e2)) e es }

expr3:
  | x = VAR { SVar(x) }
  | n = NUM { SNum(n) }
  | "true" { STrue }
  | "false" { SFalse }
  | "unit" { SUnit }
  | "(" e = expr ")" { e }

bop:
  | "+" { Add }
  | "-" { Sub }
  | "*" { Mul }
  | "/" { Div }
  | "mod" { Mod }
  | "=" { Eq }
  | "<>" { Neq }
  | "<" { Lt }
  | "<=" { Lte }
  | ">" { Gt }
  | ">=" { Gte }
  | "&&" { And }
  | "||" { Or }

arg_list:
  | LPAREN x = VAR COLON ty = ty RPAREN rest = arg_list
    { (x, ty) :: rest }
  | { [] }
