{
open Par
}

let whitespace = [' ' '\t' '\n' '\r']+
let num = '-'? ['0'-'9']+
let var = ['a'-'z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read =
  parse
  | num { NUM (int_of_string (Lexing.lexeme lexbuf)) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | whitespace { read lexbuf }
  | "let" { LET }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fun" { FUN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "unit" {UNIT}
  | '=' { EQUALS }
  | "->" { ARROW }
  | '+' { ADD }
  | '-' { SUB }
  | '*' { MUL }
  | '/' { DIV }
  | '%' { MOD }
  | "<" { LT }
  | "<=" { LTE }
  | ">" { GT }
  | ">=" { GTE }
  | "==" { EQ }
  | "!=" { NEQ }
  | "&&" { AND }
  | "||" { OR }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof { EOF }

