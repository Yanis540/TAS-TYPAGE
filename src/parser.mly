%{
open Ast (* On suppose que les types de données sont dans un fichier Ast.ml *)
%}

%token <int> INT
%token <string> IDENT
%token LET IFZERO IFEMPTY FIX ADD SUB MULT HEAD TAIL REF DEREF ASSIGN AFFECT
%token LPAREN RPAREN COMMA IN SEMICOLON LAMBDA  LBRA RBRA  THEN ELSE EOF

%left ADD SUB   
%left MULT DIV   
%right ASSIGN 

%type <Ast.pterm> prog
%type <Ast.pterm_list> pterm_list
%start prog

%%

prog:
  | expr EOF            { $1 }
;
expr:
  | INT                 { Int $1 }
  | IDENT               { Var $1 }
  | expr ADD expr  { Add ($1, $3) }
  | expr SUB expr  { Sub ($1, $3) }
  | expr MULT expr { Mult ($1, $3) }
  | LBRA pterm_list RBRA { List ($2) }
  | LAMBDA IDENT SEMICOLON expr {Abs($2,$4)}
  | HEAD expr          { Head $2 }
  | TAIL expr            { Tail $2 }
  | IFZERO LPAREN expr RPAREN THEN expr ELSE expr   { IfZero($3,$6,$8)}
  | IFEMPTY LPAREN expr RPAREN THEN expr ELSE expr   { IfEmpty($3,$6,$8)}

  | LET IDENT AFFECT expr IN expr { Let ($2, $4, $6) }
  | FIX expr                           { Fix $2 }
  | REF expr                           { Ref $2 }
  | DEREF expr                         { DeRef $2 }
  | expr ASSIGN expr                   { Assign ($1, $3) }
  | LPAREN expr RPAREN                 { $2 }
  | expr expr                 { App($1,$2) }
;
pterm_list :
  {Empty}
 | expr {Cons($1,Empty)} 
 | expr COMMA pterm_list {Cons($1,$3)} 
; 