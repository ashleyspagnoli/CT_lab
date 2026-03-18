{
  open Minifun_parser
}

rule token = parse
  | [' ' '\t' '\n' '\r'] { token lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "fun" { FUN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in" { IN }
  | "letfun" { LETFUN }
  | "int" { TINT }
  | "bool" { TBOOL }
  | "=>" { DARROW }  
  | "->" { ARROW }
  | "&&" { AND }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "<" { LT }
  | "~" { TILDE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { EQ }
  | ":" { COLON }
  | ['0'-'9']+ as n { INT (int_of_string n) }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as id { VAR id }
  | eof { EOF }
  | _ as c { failwith (Printf.sprintf "Lexer error: unexpected '%c'" c) }
