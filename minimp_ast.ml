(** MiniImp — Abstract Syntax Tree *)

(** Arithmetic expressions *)
type expr =
  | Num of int
  | Var of string
  | BinOp of string * expr * expr   (* op, left, right *)

(** Boolean expressions *)
type bexpr =
  | BoolLit of bool
  | BoolAnd of bexpr * bexpr
  | BoolNot of bexpr
  | BoolLt of expr * expr

(** Commands *)
type cmd =
  | Skip
  | Assign of string * expr
  | Seq of cmd * cmd
  | If of bexpr * cmd * cmd
  | While of bexpr * cmd

(** Program *)
type program = {
  input_var : string;
  output_var : string;
  body : cmd;
}