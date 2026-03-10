(** MiniImp — Runtime Environment and Evaluator *)

open Minimp_ast

type memory = (string * int) list

exception Deadlock of string
exception TypeError of string

let mem_get (sigma : memory) (x : string) : int =
  match List.assoc_opt x sigma with
  | Some v -> v
  | None -> raise (Deadlock (Printf.sprintf "Undefined variable: '%s'" x))

let mem_update (sigma : memory) (x : string) (n : int) : memory =
  (x, n) :: List.filter (fun (k, _) -> k <> x) sigma

(* Evaluator *)

let arith_op (op : string) (n1 : int) (n2 : int) : int =
  match op with
  | "+" -> n1 + n2
  | "-" -> n1 - n2
  | "*" -> n1 * n2
  | _ -> raise (TypeError (Printf.sprintf "Unknown arithmetic operator: %s" op))

let rec eval_expr (sigma : memory) (e : expr) : int =
  match e with
  | Num n -> n
  | Var x -> mem_get sigma x
  | BinOp (op,e1,e2) ->
    let n1 = eval_expr sigma e1 in
    let n2 = eval_expr sigma e2 in
    arith_op op n1 n2

let rec eval_bexpr (sigma : memory) (b : bexpr) : bool =
  match b with
  | BoolLit v -> v
  | BoolAnd (b1, b2) -> eval_bexpr sigma b1 && eval_bexpr sigma b2
  | BoolNot b1 -> not (eval_bexpr sigma b1)
  | BoolLt  (e1, e2) -> eval_expr sigma e1 < eval_expr sigma e2

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
    if eval_bexpr sigma b
    then
      let sigma1 = eval_cmd sigma (Seq (body, While (b, body))) in
      sigma1
    else
      sigma

let eval_program (prog : program) (input_val : int) : int =
  let sigma0 = mem_update [] prog.input_var input_val in
  let sigma1 = eval_cmd sigma0 prog.body in
  mem_get sigma1 prog.output_var