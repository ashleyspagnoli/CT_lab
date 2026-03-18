(** MiniFun — Abstract Syntax Tree *)

type binop = Add | Sub | Mul | And | Lt

type var = string

type typ = TInt | TBool | TFun of type * type

type term =
  | TNum of int (* <int> *)
  | TBool of bool (* true, false *)
  | TVar of var (* <var> *)
  | TBinOp of term * binop * term (* <t> <binop> <t> *)
  | TNot of term (* ~ <t> *)
  | TIf of term * term * term (* if <t> then <t> else <t> *)
  | TFun of var * typ * term (* fun <var> : <type> => <t> *)
  | TApp of term * term (* <t> <t> *)
  | TLet of var * term * term (* let <var> = <t> in <t> *)
  | TLetFun of var * var * typ * term * term (* letfun <var> <var> : <type> = <t> in <t> *)