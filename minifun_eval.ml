(** MiniFun — Runtime Environment and Evaluator *)

open Minifun_ast

(* Values and Closures *)

type value =
  | VInt of int
  | VBool of bool
  | VClosure of closure (* non-recursive function closures *)
  | VRecClosure of rec_closure (* recursive function closures *)

and closure = {
  cl_param : var; (* parameter name *)
  cl_body : term; (* function body *)
  cl_env : env; (* environment captured *)
}

(* Closure for recursive functions *)
and rec_closure = {
  rc_fname : var; (* function name *)
  rc_param : var; (* parameter name *)
  rc_body : term; (* function body *)
  rc_env : env ref; (* mutable environment for self-reference *)
}

(* Environment: association list mapping variable names to values *)
and env = (var * value) list

(* Runtime errors *)
exception RuntimeError of string

(* Apply a binary operator to two values *)
let apply_op (op : binop) (v1 : value) (v2 : value) : value =
  match op, v1, v2 with
  | Add, VInt n1, VInt n2 -> VInt (n1 + n2)
  | Sub, VInt n1, VInt n2 -> VInt (n1 - n2)
  | Mul, VInt n1, VInt n2 -> VInt (n1 * n2)
  | And, VBool b1, VBool b2 -> VBool (b1 && b2)
  | Lt, VInt n1, VInt n2 -> VBool (n1 < n2)
  | _, _, _ ->
      let type_of = function
        | VInt _ -> "int"
        | VBool _ -> "bool"
        | VClosure _ | VRecClosure _ -> "function"
      in
      raise (RuntimeError
        (Printf.sprintf "Operator '%s' not applicable to %s and %s"
          (match op with Add -> "+" | Sub -> "-" | Mul -> "*" | And -> "&&" | Lt -> "<")
          (type_of v1)
          (type_of v2)))

(** Memory operations *)

(* Lookup a variable in the environment *)
let env_get (rho : env) (x : var) : value =
  match List.assoc_opt x rho with
  | Some v -> v
  | None -> raise (RuntimeError (Printf.sprintf "Undefined variable: '%s'" x))

(* Extend environment with a new binding (overrides existing binding if present) *)
let env_extend (rho : env) (x : var) (v : value) : env =
  (x, v) :: List.filter (fun (k, _) -> k <> x) rho

(** Evaluator *)

let rec eval_term (rho : env) (t : term) : value =
  match t with
  | TNum n -> VInt n
  | TBool b -> VBool b
  | TVar x -> env_get rho x
  | TBinOp (t1, op, t2) ->
      let v1 = eval_term rho t1 in
      let v2 = eval_term rho t2 in
      apply_op op v1 v2
  | TNot t1 ->
      (match eval_term rho t1 with
       | VBool b -> VBool (not b)
       | _ -> raise (RuntimeError "'~' expects a boolean"))
  | TIf (t1, t2, t3) ->
      (match eval_term rho t1 with
       | VBool true -> eval_term rho t2
       | VBool false -> eval_term rho t3
       | _ -> raise (RuntimeError "If condition must be a boolean"))
  | TFun (x, body) ->
      VClosure { cl_param = x; cl_body = body; cl_env = rho }
  | TLet (x, t1, t2) ->
      let v1 = eval_term rho t1 in
      eval_term (env_extend rho x v1) t2
  | TLetFun (f, x, t1, t2) ->
      let env_ref = ref rho in
      let rc = { rc_fname = f; rc_param = x; rc_body = t1; rc_env = env_ref } in
      let rho2 = env_extend rho f (VRecClosure rc) in
      env_ref := rho2;
      eval_term rho2 t2
  | TApp (t1, t2) ->
      let v1 = eval_term rho t1 in
      let v2 = eval_term rho t2 in
      (match v1 with
       | VClosure { cl_param = x; cl_body = t0; cl_env = rho0 } ->
           eval_term (env_extend rho0 x v2) t0
       | VRecClosure ({ rc_fname = f; rc_param = x; rc_body = t0; rc_env = env_ref } as rc) ->
           let rho0 = !env_ref in
           let rho1 = env_extend (env_extend rho0 f (VRecClosure rc)) x v2 in
           eval_term rho1 t0
       | _ -> raise (RuntimeError "Application of a non-function value"))

(* Evaluate in an empty environment *)
let eval_minifun (t : term) : value =
  eval_term [] t

(* Pretty-print a value *)
let rec pp_value = function
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b
  | VClosure _ -> "<closure>"
  | VRecClosure _ -> "<rec-closure>"