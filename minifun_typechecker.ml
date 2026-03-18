(** MiniFun — Type Checker *)

open Minifun_ast

(* Typing environment: maps variable names to types *)
type env = (var * typ) list

(* Type error exception *)
exception TypeError of string

(* Look up a variable *)
let env_get (gamma : env) (x : var) : typ =
  match List.assoc_opt x gamma with
  | Some t -> t
  | None -> raise (TypeError (Printf.sprintf "Unbound variable '%s'" x))

(* Extend the context with a new binding. *)
let env_extend (gamma : env) (x : var) (t : typ) : env =
  (x, t) :: List.filter (fun (k, _) -> k <> x) gamma

(** Helpers *)

(* Pretty-print a type *)
let rec pp_typ = function
  | TInt -> "int"
  | TBool -> "bool"
  | TArrow (a, b) ->
      let l = match a with TArrow _ -> "(" ^ pp_typ a ^ ")" | _ -> pp_typ a in
      l ^ " -> " ^ pp_typ b

(* Assert two types are equal *)
let unify_simple (expected : typ) (got : typ) (ctx : string) : unit =
  if expected <> got then
    raise (TypeError(Printf.sprintf "%s: expected type '%s' but got '%s'" ctx (pp_typ expected) (pp_typ got)))

(** Type-checker *)

let rec typecheck (gamma : env) (t : term) : typ =
  match t with
  | TNum _  -> TInt
  | TBool _ -> TBool
  | TVar x  -> env_get gamma x
  | TBinOp (t1, op, t2) ->
      let ty1 = typecheck gamma t1 in
      let ty2 = typecheck gamma t2 in
      (match op with
       | Add | Sub | Mul ->
           unify_simple TInt ty1 "left operand of arithmetic operator";
           unify_simple TInt ty2 "right operand of arithmetic operator";
           TInt
       | And ->
           unify_simple TBool ty1 "left operand of '&&'";
           unify_simple TBool ty2 "right operand of '&&'";
           TBool
       | Lt ->
           unify_simple TInt ty1 "left operand of '<'";
           unify_simple TInt ty2 "right operand of '<'";
           TBool)
  | TNot t1 ->
      let ty1 = typecheck gamma t1 in
      unify_simple TBool ty1 "operand of '~'";
      TBool
  | TIf (t1, t2, t3) ->
      let ty1 = typecheck gamma t1 in
      unify_simple TBool ty1 "condition of 'if'";
      let ty2 = typecheck gamma t2 in
      let ty3 = typecheck gamma t3 in
      if ty2 <> ty3 then
        raise (TypeError(Printf.sprintf 
            "branches of 'if' must have the same type, but 'then' has type '%s' and 'else' has type '%s'" (pp_typ ty2) (pp_typ ty3)));
      ty2
  | TFun (x, _body) ->
      raise (TypeError(Printf.sprintf
          "Cannot infer the type of parameter '%s': please add a type annotation (fun %s : <type> => ...)" x x))
  | TFunA (x, ann, body) ->
      let gamma' = env_extend gamma x ann in
      let ty_body = typecheck gamma' body in
      TArrow (ann, ty_body)
  | TApp (t1, t2) ->
      let ty1 = typecheck gamma t1 in
      let ty2 = typecheck gamma t2 in
      (match ty1 with
       | TArrow (arg_ty, ret_ty) ->
           if arg_ty <> ty2 then
             raise (TypeError(Printf.sprintf
                "Function expects argument of type '%s' but got '%s'" (pp_typ arg_ty) (pp_typ ty2)));
           ret_ty
       | _ -> raise (TypeError(Printf.sprintf"Cannot apply a non-function value of type '%s'" (pp_typ ty1))))
  | TLet (x, t1, t2) ->
      let ty1 = typecheck gamma t1 in
      let gamma' = env_extend gamma x ty1 in
      typecheck gamma' t2
  | TLetFun (f, x, _body, _t2) ->
      raise (TypeError(Printf.sprintf
        "Cannot infer the type of recursive function '%s': please add a type annotation (letfun %s %s : <type> = ...)" f f x))
  | TLetFunA (f, x, ann, body, t2) ->
      (match ann with
       | TArrow (arg_ty, ret_ty) ->
          let gamma_body =
             gamma
             |> (fun g -> env_extend g f ann)
             |> (fun g -> env_extend g x arg_ty)
           in
           let ty_body = typecheck gamma_body body in
           if ty_body <> ret_ty then
             raise (TypeError(Printf.sprintf
                "Body of 'letfun %s' has type '%s' but annotation declares return type '%s'" f (pp_typ ty_body) (pp_typ ret_ty)));
           let gamma_cont = env_extend gamma f ann in
           typecheck gamma_cont t2
       | _ ->
           raise (TypeError(Printf.sprintf
              "Annotation on 'letfun %s' must be a function type (τ → τ'), got '%s'" f (pp_typ ann))))

let typecheck_program (t : term) : typ =
  typecheck [] t