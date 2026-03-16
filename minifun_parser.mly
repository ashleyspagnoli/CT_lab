%{
  open Minifun_ast
%}

%token TRUE FALSE
%token FUN IF THEN ELSE LET IN LETFUN
%token ARROW AND PLUS MINUS STAR LT TILDE EQ
%token LPAREN RPAREN
%token <string> VAR
%token <int> INT
%token EOF

%right IN
%right ELSE
%right ARROW
%left AND PLUS MINUS STAR
%nonassoc LT
%right TILDE

%start term_eof
%type <Minifun_ast.term> term_eof

%%

term_eof:
  | term EOF { $1 }
;

term:
  | FUN VAR ARROW term { TFun ($2, $4) }
  | IF term THEN term ELSE term { TIf ($2, $4, $6) }
  | LET VAR EQ term IN term { TLet ($2, $4, $6) }
  | LETFUN VAR VAR EQ term IN term { TLetFun ($2, $3, $5, $7) }
  | term PLUS term { TBinOp ($1, Add, $3) }
  | term MINUS term { TBinOp ($1, Sub, $3) }
  | term STAR term { TBinOp ($1, Mul, $3) }
  | term AND term { TBinOp ($1, And, $3) }
  | term LT term { TBinOp ($1, Lt, $3) }
  | TILDE term { TNot $2 }
  | app { $1 }
; 

app:
  | app atom { TApp ($1, $2) }
  | atom { $1 }
;

atom:
  | INT { TNum $1 }
  | TRUE { TBool true }
  | FALSE { TBool false }
  | VAR { TVar $1 }
  | LPAREN term RPAREN { $2 }
;
