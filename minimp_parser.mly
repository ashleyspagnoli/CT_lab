%{
  open Minimp_ast
%}

%token DEF MAIN WITH INPUT OUTPUT AS
%token SKIP IF THEN ELSE WHILE DO
%token TRUE FALSE AND NOT
%token ASSIGN SEMI LPAREN RPAREN
%token PLUS MINUS STAR LT
%token <string> VAR
%token <int>    INT
%token EOF

%right ELSE
%left  SEMI
%left  AND
%right NOT
%nonassoc LT
%left  PLUS MINUS
%left  STAR

%start program
%type <Minimp_ast.program> program

%%

program:
  | DEF MAIN WITH INPUT VAR OUTPUT VAR AS cmd EOF
    { { input_var = $5; output_var = $7; body = $9 } }
;

cmd:
  | LPAREN cmd RPAREN { $2 }
  | SKIP { Skip }
  | VAR ASSIGN expr { Assign ($1, $3) }
  | cmd SEMI cmd { Seq ($1, $3) }
  | IF bexpr THEN cmd ELSE cmd { If ($2, $4, $6) }
  | WHILE bexpr DO cmd { While ($2, $4) }
;

expr:
  | INT { Num $1 }
  | VAR { Var $1 }
  | expr PLUS  expr { BinOp ("+", $1, $3) }
  | expr MINUS expr { BinOp ("-", $1, $3) }
  | expr STAR  expr { BinOp ("*", $1, $3) }
  | LPAREN expr RPAREN { $2 }
;

bexpr:
  | TRUE { BoolLit true }
  | FALSE { BoolLit false }
  | bexpr AND bexpr { BoolAnd ($1, $3) }
  | NOT bexpr { BoolNot $2 }
  | expr LT expr { BoolLt ($1, $3) }
  | LPAREN bexpr RPAREN { $2 }
;
