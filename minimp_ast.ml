(** MiniImp — Abstract Syntax Tree *)

type op = Add | Sub | Mul

(** Arithmetic expressions *)
type expr =
  | Var of string (* <var> *)
  | Num of int (* <int> *)
  | BinOp of expr * op * expr (* op, left, right: <e> + <e> | <e> - <e> | <e> * <e> *)

(** Boolean expressions *)
type bexpr =
  | BoolLit of bool (* true, false *)
  | BoolAnd of bexpr * bexpr (* <b> and <b> *)
  | BoolNot of bexpr (* not <b> *)
  | BoolLt of expr * expr (* <e> < <e> *)

(** Commands *)
type cmd =
  | Skip (* skip *)
  | Assign of string * expr (* <var> := <e> *)
  | Seq of cmd * cmd (* <cmd> ; <cmd> *)
  | If of bexpr * cmd * cmd (* if <b> then <cmd> else <cmd> *)
  | While of bexpr * cmd (* while <b> do <cmd> *)

(** Program *)
(* prog ::= def main with input <var> output <var> as <cmd> *)
type program = {
  input_var : string;
  output_var : string;
  body : cmd;
}