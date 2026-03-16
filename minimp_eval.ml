(** MiniImp — Runtime Environment and Evaluator *)

open Minimp_ast

(** Memory model: we represent memory as an association list from variable names to integer values *)
type memory = (string * int) list

(** Exceptions *)
exception Deadlock of string
exception TypeError of string

(** Memory operations *)

(* Retrieve a variable value from memory. If the variable is not found, raise Error. *)
let mem_get (sigma : memory) (x : string) : int =
  match List.assoc_opt x sigma with
  | Some v -> v
  | None -> raise (Deadlock (Printf.sprintf "Undefined variable: '%s'" x))

(* Update a variable in memory. Overwrite if it exists, else add it. *)
let mem_update (sigma : memory) (x : string) (n : int) : memory =
  (x, n) :: List.filter (fun (k, _) -> k <> x) sigma

(** Evaluator *)

(* Evaluate an arithmetic operator on two integers. *)
let arith_op (op : op) (n1 : int) (n2 : int) : int =
  match op with
  | Add -> n1 + n2
  | Sub -> n1 - n2
  | Mul -> n1 * n2

(* Evaluate arithmetic expressions *)
let rec eval_expr (sigma : memory) (e : expr) : int =
  match e with
  | Num n -> n
  | Var x -> mem_get sigma x
  | BinOp (e1, op, e2) ->
      let n1 = eval_expr sigma e1 in
      let n2 = eval_expr sigma e2 in
      arith_op op n1 n2

(* Evaluate boolean expressions *)
let rec eval_bexpr (sigma : memory) (b : bexpr) : bool =
  match b with
  | BoolLit v -> v
  | BoolAnd (b1, b2) -> eval_bexpr sigma b1 && eval_bexpr sigma b2
  | BoolNot b1 -> not (eval_bexpr sigma b1)
  | BoolLt (e1, e2) -> eval_expr sigma e1 < eval_expr sigma e2

(* Evaluate commands. Returns the updated memory *)
let rec eval_cmd (sigma : memory) (c : cmd) : memory =
  match c with
  | Skip -> sigma
  | Assign (x, e) ->
      let n = eval_expr sigma e in
      mem_update sigma x n
  | Seq (c1, c2) ->
      let sigma1 = eval_cmd sigma c1 in
      eval_cmd sigma1 c2
  | If (b, c1, c2) ->
      if eval_bexpr sigma b
      then eval_cmd sigma c1
      else eval_cmd sigma c2
  | While (b, body) ->
      if eval_bexpr sigma b then
        let sigma1 = eval_cmd sigma (Seq (body, While (b, body))) in
        sigma1
      else sigma

(* Evaluate a program *)
let eval_program (prog : program) (input_val : int) : int =
  let sigma0 = mem_update [] prog.input_var input_val in
  let sigma1 = eval_cmd sigma0 prog.body in
  mem_get sigma1 prog.output_var