(** MiniImp — Optimizations *)

open Minimp_ast
open Minimp_cfg
open Minimp_dataflow

(** Undefined variable checking *)

let check_undefined (prog : program) (g : plain_cfg) : (int * string) list =
  let (_def_cfg, def_warnings) = analyse_defined prog g in
  let live_cfg = analyse_live prog g in
  let entry_node = Hashtbl.find live_cfg.nodes live_cfg.entry in
  let live_at_entry = SS.remove prog.input_var entry_node.ann.df_in in
  let live_warnings = SS.fold (fun v acc -> (live_cfg.entry, v) :: acc) live_at_entry [] in
  List.sort_uniq compare (def_warnings @ live_warnings)

(** Dead Store Elimination *)

let stmt_live_sets (code : block) (block_out : SS.t) : SS.t list =
  let n = List.length code in
  let arr = Array.make n SS.empty in
  let stmts = Array.of_list code in
  let live = ref block_out in
  for i = n - 1 downto 0 do
    arr.(i) <- !live;
    let s = stmts.(i) in
    let used = used_stmt s in
    let defd = assigned_stmt s in
    live := SS.union used (SS.diff !live defd)
  done;
  Array.to_list arr

let eliminate_dead_stores (prog : program) (g : plain_cfg) : plain_cfg =
  let live_cfg = analyse_live prog g in
  let new_nodes : (int, unit node) Hashtbl.t = Hashtbl.create (Hashtbl.length g.nodes) in
  Hashtbl.iter (fun id n ->
    let block_out = (Hashtbl.find live_cfg.nodes id).ann.df_out in
    let live_afters = stmt_live_sets n.code block_out in
    let new_code =
      List.filter_map (fun (s, live_after) ->
        match s with
        | SAssign (x, _) ->
            if SS.mem x live_after then Some s
            else None (* drop dead store *)
        | _ -> Some s
      ) (List.combine n.code live_afters)
    in
    (* SSkip preserved so the block is not empty *)
    let new_code = if new_code = [] then [SSkip] else new_code in
    Hashtbl.add new_nodes id { n with code = new_code; ann = () }
  ) g.nodes;
  { g with nodes = new_nodes }


(** Constant Folding *)

let rec try_eval_expr (e : expr) : int option =
  match e with
  | Num n -> Some n
  | Var _ -> None
  | BinOp (l, op, r) ->
      match try_eval_expr l, try_eval_expr r with
      | Some n1, Some n2 ->
          (match op with
           | Add -> Some (n1 + n2)
           | Sub -> Some (n1 - n2)
           | Mul -> Some (n1 * n2))
      | _ -> None

let rec try_eval_bexpr (b : bexpr) : bool option =
  match b with
  | BoolLit v -> Some v
  | BoolNot b1 -> (match try_eval_bexpr b1 with Some v -> Some (not v) | None -> None)
  | BoolAnd (b1, b2) ->
      (match try_eval_bexpr b1, try_eval_bexpr b2 with
       | Some v1, Some v2 -> Some (v1 && v2)
       | Some false, _ | _, Some false -> Some false
       | _ -> None)
  | BoolLt (e1, e2) ->
      (match try_eval_expr e1, try_eval_expr e2 with
       | Some n1, Some n2 -> Some (n1 < n2)
       | _ -> None)

let rec fold_expr (e : expr) : expr =
  match e with
  | Num _ | Var _ -> e
  | BinOp (l, op, r) ->
      let l' = fold_expr l and r' = fold_expr r in
      (match try_eval_expr (BinOp (l', op, r')) with
       | Some n -> Num n
       | None ->
           (match op, l', r' with
            | Add, Num 0, x | Add, x, Num 0 -> x (* 0+x = x+0 = x *)
            | Sub, x, Num 0 -> x (* x-0 = x *)
            | Mul, Num 1, x | Mul, x, Num 1 -> x (* 1*x = x*1 = x *)
            | Mul, Num 0, _ | Mul, _, Num 0 -> Num 0 (* 0*x = x*0 = 0 *)
            | Sub, x, y when x = y -> Num 0 (* x-x = 0 *)
            | _ -> BinOp (l', op, r')))

let rec fold_bexpr (b : bexpr) : bexpr =
  match b with
  | BoolLit _ -> b
  | BoolNot b1 ->
      let b1' = fold_bexpr b1 in
      (match try_eval_bexpr b1' with Some v -> BoolLit v | None -> BoolNot b1')
  | BoolAnd (b1, b2) ->
      let b1' = fold_bexpr b1 and b2' = fold_bexpr b2 in
      (match try_eval_bexpr (BoolAnd (b1', b2')) with
       | Some v -> BoolLit v
       | None ->
           (match b1', b2' with
            | BoolLit true, x | x, BoolLit true -> x (* true and x = x *)
            | BoolLit false, _ | _, BoolLit false -> BoolLit false
            | _ -> BoolAnd (b1', b2')))
  | BoolLt (e1, e2) ->
      let e1' = fold_expr e1 and e2' = fold_expr e2 in
      (match try_eval_bexpr (BoolLt (e1', e2')) with
       | Some v -> BoolLit v
       | None -> BoolLt (e1', e2'))

let fold_stmt (s : stmt) : stmt =
  match s with
  | SSkip -> SSkip
  | SAssign (x, e) -> SAssign (x, fold_expr e)
  | SGuard b -> SGuard (fold_bexpr b)

let constant_folding (g : plain_cfg) : plain_cfg =
  let new_nodes : (int, unit node) Hashtbl.t = Hashtbl.create (Hashtbl.length g.nodes) in
  Hashtbl.iter (fun id n ->
    let new_code = List.map fold_stmt n.code in
    Hashtbl.add new_nodes id { n with code = new_code; ann = () }
  ) g.nodes;
  { g with nodes = new_nodes }


(** Constant Propagation *)

type const_env = (string * int option) list

let env_find (env : const_env) (x : string) : int option option =
  match List.assoc_opt x env with
  | Some v -> Some v
  | None -> None

let env_set (env : const_env) (x : string) (v : int option) : const_env =
  (x, v) :: List.filter (fun (k, _) -> k <> x) env

let rec prop_expr (env : const_env) (e : expr) : expr =
  match e with
  | Num _ -> e
  | Var x ->
      (match env_find env x with
       | Some (Some n) -> Num n
       | _ -> e)
  | BinOp (l, op, r) -> BinOp (prop_expr env l, op, prop_expr env r)

let rec prop_bexpr (env : const_env) (b : bexpr) : bexpr =
  match b with
  | BoolLit _ -> b
  | BoolNot b1 -> BoolNot (prop_bexpr env b1)
  | BoolAnd (b1, b2) -> BoolAnd (prop_bexpr env b1, prop_bexpr env b2)
  | BoolLt (e1, e2) -> BoolLt (prop_expr env e1, prop_expr env e2)

let prop_stmt (env : const_env) (s : stmt) : stmt * const_env =
  match s with
  | SSkip -> (SSkip, env)
  | SGuard b -> (SGuard (prop_bexpr env b), env)
  | SAssign (x, e) ->
      let e' = prop_expr env e in
      let v = try_eval_expr e' in
      let e'' = (match v with Some n -> Num n | None -> e') in
      (SAssign (x, e''), env_set env x v)

let build_env_from_reach (df_in : IS.t) (all_defs : def_site list) (g : plain_cfg) : const_env =
  let by_var : (string, def_site list) Hashtbl.t = Hashtbl.create 8 in
  IS.iter (fun did ->
    match List.find_opt (fun d -> d.def_id = did) all_defs with
    | None -> ()
    | Some d ->
        let prev = match Hashtbl.find_opt by_var d.def_var with
          | Some l -> l | None -> [] in
        Hashtbl.replace by_var d.def_var (d :: prev)
  ) df_in;
  Hashtbl.fold (fun x defs acc ->
    let const_values = List.filter_map (fun d ->
      if d.def_node = -1 then None
      else
        match Hashtbl.find_opt g.nodes d.def_node with
        | None -> None
        | Some n ->
            (match List.nth_opt n.code d.def_idx with
             | Some (SAssign (_, e)) -> try_eval_expr e
             | _ -> None)
    ) defs in
    let value =
      if List.length const_values = List.length defs then
        match const_values with
        | [] -> None
        | v :: rest ->
            if List.for_all (( = ) v) rest then Some v
            else None
      else None
    in
    env_set acc x value
  ) by_var []

let constant_propagation (prog : program) (g : plain_cfg) : plain_cfg =
  let (reach_cfg, all_defs, _var_to_defs) = analyse_reaching prog g in
  let new_nodes : (int, unit node) Hashtbl.t = Hashtbl.create (Hashtbl.length g.nodes) in
  Hashtbl.iter (fun id n ->
    let reach_node = Hashtbl.find reach_cfg.nodes id in
    let init_env = build_env_from_reach reach_node.ann.df_in all_defs g in
    let new_code, _ =
      List.fold_left (fun (acc, env) s ->
        let (s', env') = prop_stmt env s in
        (acc @ [s'], env')
      ) ([], init_env) n.code
    in
    Hashtbl.add new_nodes id { n with code = new_code; ann = () }
  ) g.nodes;
  { g with nodes = new_nodes }


(** Optimization pipeline *) 

let cfg_fingerprint (g : plain_cfg) : string =
  let ids = Hashtbl.fold (fun id _ acc -> id :: acc) g.nodes [] |> List.sort compare in
  String.concat "|" (List.map (fun id ->
    let n = Hashtbl.find g.nodes id in
    Printf.sprintf "%d:%s:%s" id (pp_block n.code) (pp_next n.next)
  ) ids)

let optimise (prog : program) (g : plain_cfg) : plain_cfg =
  let rec fix_point g =
    let before = cfg_fingerprint g in
    let g = constant_folding g in
    let g = constant_propagation prog g in
    let after = cfg_fingerprint g in
    if before = after then g else fix_point g
  in
  let g = fix_point g in
  let g = eliminate_dead_stores prog g in
  let g = constant_folding g in
  g

(** Pretty-printers *)

let pp_undefined_warnings (warnings : (int * string) list) : string =
  if warnings = [] then "No undefined variable warnings."
  else
    String.concat "\n" (List.map (fun (blk, v) ->
      Printf.sprintf "Warning: variable '%s' may be used undefined (block %d)" v blk
    ) warnings)