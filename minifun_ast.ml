(** MiniFun — Abstract Syntax Tree *)

type binop = Add | Sub | Mul | And | Lt

type var = string

type term =
  | TNum of int (* <int> *)
  | TBool of bool (* true, false *)
  | TVar of var (* <var> *)
  | TBinOp of term * binop * term (* <t> <binop> <t> *)
  | TNot of term (* ~ <t> *)
  | TIf of term * term * term (* if <t> then <t> else <t> *)
  | TFun of var * term (* fun <var> => <t> *)
  | TApp of term * term (* <t> <t> *)
  | TLet of var * term * term (* let <var> = <t> in <t> *)
  | TLetFun of var * var * term * term (* letfun <var> <var> = <t> in <t> *)