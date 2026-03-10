{
  open Minimp_parser
}

rule token = parse
  | [' ' '\t' '\n' '\r'] { token lexbuf }
  | "def" { DEF }
  | "main" { MAIN }
  | "with" { WITH }
  | "input" { INPUT }
  | "output" { OUTPUT }
  | "as" { AS }
  | "skip" { SKIP }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "do" { DO }
  | "true" { TRUE }
  | "false" { FALSE }
  | "and" { AND }
  | "not" { NOT }
  | ":=" { ASSIGN }
  | ";" { SEMI }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { STAR }
  | "<" { LT }
  | ['0'-'9']+ as n { INT (int_of_string n) }
  | ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as id { VAR id }
  | eof { EOF }
  | _ as c { failwith (Printf.sprintf "Lexer error: unexpected character '%c'" c) }