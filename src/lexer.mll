
{
  open Parser        (* The type token is defined in parser.mli *)

}
rule token = parse
    [' ' '\t' '\n' '\r']       { token lexbuf }     (* skip blanks *)
  | "let"                   { LET }
  | "in"                   { IN }
  | "ifZero"                    { IFZERO }
  | "then"                    { THEN }
  | "else"                    { ELSE }
  | "ifEmpty"                    { IFEMPTY }
  | "fix"                   { FIX }
  | "lambda"                   { LAMBDA }
  | ":"                   { SEMICOLON }
  | "+"                   { ADD }
  | "-"                   { SUB }
  | "*"                  { MULT }
  | "hd"                  { HEAD }
  | "tail"                  { TAIL }
  | "ref"                   { REF }
  | "!"                     { DEREF }
  | ":="                    { ASSIGN }
  | "="                    { AFFECT }
  | "["                    { LBRA }
  | "]"                    { RBRA }
  | ['0'-'9']+ as num       { INT (int_of_string num) }
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as ident { IDENT ident }
  | '('                     { LPAREN }
  | ')'                     { RPAREN }
  | ','                     { COMMA }
  | eof              {EOF}