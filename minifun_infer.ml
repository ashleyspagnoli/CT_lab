(** MiniFun Hindley-Milner Type Inference (Algorithm W) *)

open Minifun_ast

(** Data types *)

type tvar = string

type monotype = MVar of tvar | MInt | MBool | MArrow of monotype * monotype

type polytype = Mono of monotype | Forall of tvar * polytype

exception TypeError of string

(* Fresh variable generator *)

let fresh_counter = ref 0  (* mutable variable *)

let fresh_var () : tvar =
  let n = !fresh_counter in (* read reference value *)
  incr fresh_counter;
  "a" ^ string_of_int n (* concatenates "a" and n *)

let reset_fresh () = fresh_counter := 0

(** Substitutions: represented as association lists *)

type subst = (tvar * monotype) list

let empty_subst : subst = []

(* Look up a variable in a substitution *)
let subst_lookup (s : subst) (v : tvar) : monotype =
  match List.assoc_opt v s with
  | Some t -> t
  | None -> MVar v

(* Apply substitution to a monotype *)
let rec apply_subst_mono (s : subst) (m : monotype) : monotype =
  match m with
  | MInt -> MInt
  | MBool -> MBool
  | MVar v -> subst_lookup s v
  | MArrow (a, b) -> MArrow (apply_subst_mono s a, apply_subst_mono s b)

(* Apply substitution to a polytype *)
let rec apply_subst_poly (s : subst) (p : polytype) : polytype =
  match p with
  | Mono m -> Mono (apply_subst_mono s m)
  | Forall (v, body) ->
      let s' = List.filter (fun (k, _) -> k <> v) s in (* Remove v from s so we don't capture the bound variable *)
      Forall (v, apply_subst_poly s' body)

(* Compose two substitutions *)
let compose_subst (s2 : subst) (s1 : subst) : subst =
  let s1' = List.map (fun (v, t) -> (v, apply_subst_mono s2 t)) s1 in
  let extra = List.filter (fun (v, _) -> not (List.mem_assoc v s1')) s2 in
  s1' @ extra

(** Typing environment *)

type env = (var * polytype) list

let env_get (gamma : env) (x : var) : polytype =
  match List.assoc_opt x gamma with
  | Some p -> p
  | None -> raise (TypeError (Printf.sprintf "Unbound variable '%s'" x))

let env_extend (gamma : env) (x : var) (p : polytype) : env =
  (x, p) :: List.filter (fun (k, _) -> k <> x) gamma

(* Apply a substitution to every type in the environment *)
let apply_subst_env (s : subst) (gamma : env) : env =
  List.map (fun (x, p) -> (x, apply_subst_poly s p)) gamma

(** Free type variables *)

let rec ftv_mono (m : monotype) : tvar list =
  match m with
  | MInt | MBool -> []
  | MVar v -> [v]
  | MArrow (a, b) -> ftv_mono a @ ftv_mono b

let rec ftv_poly (p : polytype) : tvar list =
  match p with
  | Mono m -> ftv_mono m
  | Forall (v, body) -> List.filter (fun x -> x <> v) (ftv_poly body)

let ftv_env (gamma : env) : tvar list =
  List.concat_map (fun (_, p) -> ftv_poly p) gamma

(* Replaces all bound variables with fresh type variables *)
let inst (p : polytype) : monotype =
  let rec go subst = function
    | Mono m -> apply_subst_mono subst m
    | Forall (v, body) ->
        let fresh = MVar (fresh_var ()) in
        let subst' = (v, fresh) :: subst in
        go subst' body
  in
  go [] p

(* Quantifies over free variables not appearing in the context *)
let gener (gamma : env) (m : monotype) : polytype =
  let env_vars = ftv_env gamma in
  let free_vars = List.filter (fun v -> not (List.mem v env_vars)) (ftv_mono m)
  in
  let rec unique lst =
    match lst with
    | [] -> []
    | x :: xs -> x :: unique (List.filter (fun y -> y <> x) xs)
  in
  let unique_vars = unique free_vars in
  List.fold_right (fun v acc -> Forall (v, acc)) unique_vars (Mono m)

(** Unification *)

(* Checks if a type variable v appears inside a monotype m *)
let rec occurs (v : tvar) (m : monotype) : bool =
  match m with
  | MInt | MBool -> false
  | MVar w -> v = w
  | MArrow (a, b) -> occurs v a || occurs v b

(* Unify: returns the most general substitution S such that S t1 = S t2 *)
let rec unify (t1 : monotype) (t2 : monotype) : subst =
  match t1, t2 with
  | MInt, MInt -> empty_subst
  | MBool, MBool -> empty_subst
  | MVar v, MVar w when v = w -> empty_subst
  | MVar v, t | t, MVar v ->
      if occurs v t then
        raise (TypeError(Printf.sprintf"Occurs check failed: type variable '%s' would create an infinite type" v))
      else
        [(v, t)]
  | MArrow (a1, b1), MArrow (a2, b2) ->
      let s1 = unify a1 a2 in
      let s2 = unify (apply_subst_mono s1 b1) (apply_subst_mono s1 b2) in
      compose_subst s2 s1
  | _ ->
      raise (TypeError(Printf.sprintf 
        "Type mismatch: cannot unify '%s' with '%s'" (pp_mono t1) (pp_mono t2)))

(** Pretty-print a monotype *)
and pp_mono = function
  | MInt -> "int"
  | MBool -> "bool"
  | MVar v -> v
  | MArrow (a, b) ->
      let l = match a with MArrow _ -> "(" ^ pp_mono a ^ ")" | _ -> pp_mono a in
      l ^ " -> " ^ pp_mono b

let rec pp_poly = function
  | Mono m -> pp_mono m
  | Forall (v, p) -> Printf.sprintf "∀%s. %s" v (pp_poly p)

(** Algorithm W *)

let rec alg_w (gamma : env) (t : term) : monotype * subst =
  match t with
  | TNum _  -> (MInt,  empty_subst)
  | TBool _ -> (MBool, empty_subst)
  | TVar x ->
      let sigma = env_get gamma x in
      (inst sigma, empty_subst)
  | TFun (x, body) ->
      let tau = MVar (fresh_var ()) in
      let gamma' = env_extend gamma x (Mono tau) in
      let (tau', s) = alg_w gamma' body in
      (MArrow (apply_subst_mono s tau, tau'), s)
  | TFunA (x, ann, body) ->
      let tau_ann = mono_of_typ ann in
      let gamma'  = env_extend gamma x (Mono tau_ann) in
      let (tau', s) = alg_w gamma' body in
      (MArrow (apply_subst_mono s tau_ann, tau'), s)
  | TApp (t1, t2) ->
      let (tau1, s1) = alg_w gamma t1 in
      let (tau2, s2) = alg_w (apply_subst_env s1 gamma) t2 in
      let tau' = MVar (fresh_var ()) in
      let s3 = unify (apply_subst_mono s2 tau1) (MArrow (tau2, tau')) in
      (apply_subst_mono s3 tau', compose_subst s3 (compose_subst s2 s1))
  | TBinOp (t1, op, t2) ->
      let (tau1, s1) = alg_w gamma t1 in
      let (tau2, s2) = alg_w (apply_subst_env s1 gamma) t2 in
      (match op with
       | Add | Sub | Mul ->
           let s3 = unify (apply_subst_mono s2 tau1) MInt in
           let s4 = unify (apply_subst_mono s3 tau2) MInt in
           (MInt, compose_subst s4 (compose_subst s3 (compose_subst s2 s1)))
       | And ->
           let s3 = unify (apply_subst_mono s2 tau1) MBool in
           let s4 = unify (apply_subst_mono s3 tau2) MBool in
           (MBool, compose_subst s4 (compose_subst s3 (compose_subst s2 s1)))
       | Lt ->
           let s3 = unify (apply_subst_mono s2 tau1) MInt in
           let s4 = unify (apply_subst_mono s3 tau2) MInt in
           (MBool, compose_subst s4 (compose_subst s3 (compose_subst s2 s1))))
  | TNot t1 ->
      let (tau1, s1) = alg_w gamma t1 in
      let s2 = unify tau1 MBool in
      (MBool, compose_subst s2 s1)
  | TIf (t1, t2, t3) ->
      let (tau1, s1) = alg_w gamma t1 in
      let (tau2, s2) = alg_w (apply_subst_env s1 gamma) t2 in
      let (tau3, s3) = alg_w (apply_subst_env s2 gamma) t3 in
      let s4 = unify (apply_subst_mono (compose_subst s3 s2) tau1) MBool in
      let s5 = unify
                 (apply_subst_mono (compose_subst s4 s3) tau2)
                 (apply_subst_mono s4 tau3)
      in
      let full_s = compose_subst s5
                    (compose_subst s4
                      (compose_subst s3
                        (compose_subst s2 s1)))
      in
      (apply_subst_mono (compose_subst s5 s4) tau3, full_s)ì
  | TLet (x, t1, t2) ->
      let (tau1, s1) = alg_w gamma t1 in
      let gamma1 = apply_subst_env s1 gamma in
      let sigma = gener gamma1 tau1 in
      let gamma1' = env_extend gamma1 x sigma in
      let (tau2, s2) = alg_w gamma1' t2 in
      (tau2, compose_subst s2 s1)
  | TLetFun (f, x, body, t2) ->
      let tau_f = MVar (fresh_var ()) in
      let tau_x = MVar (fresh_var ()) in
      let gamma' =
        gamma
        |> (fun g -> env_extend g f (Mono tau_f))
        |> (fun g -> env_extend g x (Mono tau_x))
      in
      let (tau1, s1) = alg_w gamma' body in
      let s2 = unify
                 (apply_subst_mono s1 tau_f)
                 (MArrow (apply_subst_mono s1 tau_x, tau1))
      in
      let s21 = compose_subst s2 s1 in
      let gamma_cont =
        apply_subst_env s21 gamma
        |> (fun g -> env_extend g f (Mono (apply_subst_mono s21 tau_f)))
      in
      let (tau2, s3) = alg_w gamma_cont t2 in
      (tau2, compose_subst s3 s21)
  | TLetFunA (f, x, ann, body, t2) ->
      let tau_ann = mono_of_typ ann in
      let (tau_x, tau_ret) =
        match tau_ann with
        | MArrow (a, b) -> (a, b)
        | _ ->
            raise (TypeError
              (Printf.sprintf
                 "Annotation on 'letfun %s' must be a function type, got '%s'"
                 f (pp_mono tau_ann)))
      in
      let gamma' =
        gamma
        |> (fun g -> env_extend g f (Mono tau_ann))
        |> (fun g -> env_extend g x (Mono tau_x))
      in
      let (tau1, s1) = alg_w gamma' body in
      let s2 = unify (apply_subst_mono s1 tau1) (apply_subst_mono s1 tau_ret) in
      let s21 = compose_subst s2 s1 in
      let gamma_cont =
        apply_subst_env s21 gamma
        |> (fun g -> env_extend g f (Mono (apply_subst_mono s21 tau_ann)))
      in
      let (tau2, s3) = alg_w gamma_cont t2 in
      (tau2, compose_subst s3 s21)

and mono_of_typ : typ -> monotype = function
  | TInt -> MInt
  | TBool -> MBool
  | TArrow (a, b) -> MArrow (mono_of_typ a, mono_of_typ b)

let infer_program (t : term) : polytype =
  reset_fresh ();
  let (tau, s) = alg_w [] t in
  gener (apply_subst_env s []) (apply_subst_mono s tau)

let infer_program_pp (t : term) : string =
  pp_poly (infer_program t)