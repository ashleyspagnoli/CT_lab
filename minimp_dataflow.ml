(** MiniImp — Data-Flow Analysis *)

open Minimp_ast
open Minimp_cfg

type 'a df_ann = { df_in : 'a; df_out : 'a }

type 'a df_cfg = ('a df_ann) cfg

type direction = Forward | Backward

(* String and integers sets *)
module SS = Set.Make(String)
module IS = Set.Make(Int)

(** Support functions *)

(* Variables mentioned (read) in an expression *)
let rec vars_expr (e : expr) : SS.t =
  match e with
  | Num _ -> SS.empty
  | Var x -> SS.singleton x
  | BinOp (e1, _, e2) -> SS.union (vars_expr e1) (vars_expr e2)

(* Variables mentioned in a boolean expression *)
let rec vars_bexpr (b : bexpr) : SS.t =
  match b with
  | BoolLit _ -> SS.empty
  | BoolNot b1 -> vars_bexpr b1
  | BoolAnd (b1, b2) -> SS.union (vars_bexpr b1) (vars_bexpr b2)
  | BoolLt (e1, e2) -> SS.union (vars_expr e1) (vars_expr e2)

(* Variables assigned *)
let assigned_stmt (s : stmt) : SS.t =
  match s with
  | SAssign (x, _) -> SS.singleton x
  | SSkip | SGuard _ -> SS.empty

(* Variables used (read before being assigned) *)
let used_stmt (s : stmt) : SS.t =
  match s with
  | SSkip -> SS.empty
  | SAssign (_, e) -> vars_expr e
  | SGuard b -> vars_bexpr b

(** Defined Variables *)

(* Gen set for a block *)
let gen_defined (code : block) : SS.t =
  List.fold_left (fun acc s -> SS.union acc (assigned_stmt s)) SS.empty code

(* Transfer function *)
let transfer_defined (code : block) (df_in : SS.t) : SS.t =
  SS.union (gen_defined code) df_in

(* Defined-variables analysis *)
let analyse_defined (prog : program) (g : plain_cfg) : SS.t df_cfg * (int * string) list =
  let all_vars = 
    Hashtbl.fold (
      fun _ n acc -> List.fold_left (fun a s -> SS.union a (SS.union (used_stmt s) (assigned_stmt s))) acc n.code
    ) g.nodes SS.empty in
  let top = all_vars in
  let ann0 = { df_in = top; df_out = SS.empty } in
  let cfg : SS.t df_cfg = annotate_cfg ann0 g in
  (match Hashtbl.find_opt cfg.nodes cfg.entry with
   | None -> ()
   | Some n -> let entry_in = SS.singleton prog.input_var in
              Hashtbl.replace cfg.nodes cfg.entry { n with ann = { n.ann with df_in = entry_in } });
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun id ->
      let n = Hashtbl.find cfg.nodes id in
      let new_in =
        if id = cfg.entry then n.ann.df_in
        else begin
          let preds = predecessors cfg id in
          match preds with
          | [] -> SS.empty
          | _ -> List.fold_left(fun acc pid -> let pn = Hashtbl.find cfg.nodes pid in SS.inter acc pn.ann.df_out) top preds
        end
      in
      let new_out = transfer_defined n.code new_in in
      if not (SS.equal new_in n.ann.df_in) || not (SS.equal new_out n.ann.df_out) then begin
        Hashtbl.replace cfg.nodes id { n with ann = { df_in = new_in; df_out = new_out } };
        changed := true
      end
    ) (sorted_ids cfg)
  done;

  (* Collect warnings *)
  let warnings =
    Hashtbl.fold (fun id n acc ->
      let rec check stmts locally_defined acc =
        match stmts with
        | [] -> acc
        | s :: rest ->
            let used = used_stmt s in
            let definitely_defined = SS.union n.ann.df_in locally_defined in
            let undef = SS.diff used definitely_defined in
            let acc' = SS.fold (fun v a -> (id, v) :: a) undef acc in
            let newly_defined = assigned_stmt s in
            check rest (SS.union locally_defined newly_defined) acc'
      in
      check n.code SS.empty acc
    ) cfg.nodes []
  in
  (cfg, List.sort_uniq compare warnings)

(** Live Variables *)

(* Compute Gen and Kill for a block *)
let gen_kill_live (code : block) : SS.t * SS.t =
  List.fold_left (fun (gen, kill) s ->
    let used = used_stmt s in
    let defd = assigned_stmt s in
    let new_gen = SS.union gen (SS.diff used kill) in
    let new_kill = SS.union kill defd in
    (new_gen, new_kill)
  ) (SS.empty, SS.empty) code

(* Transfer function *)
let transfer_live (code : block) (df_out : SS.t) : SS.t =
  let (gen, kill) = gen_kill_live code in
  SS.union gen (SS.diff df_out kill)

(* Live-variables analysis *)
let analyse_live (prog : program) (g : plain_cfg) : SS.t df_cfg =
  let ann0 = { df_in = SS.empty; df_out = SS.empty } in
  let cfg : SS.t df_cfg = annotate_cfg ann0 g in
  (match Hashtbl.find_opt cfg.nodes cfg.exit with
   | None -> ()
   | Some n ->
       let exit_out = SS.singleton prog.output_var in
       Hashtbl.replace cfg.nodes cfg.exit { n with ann = { n.ann with df_out = exit_out } });

  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun id ->
      let n = Hashtbl.find cfg.nodes id in
      let new_out =
        if id = cfg.exit then n.ann.df_out
        else List.fold_left(fun acc sid -> let sn = Hashtbl.find cfg.nodes sid in 
                                          SS.union acc sn.ann.df_in) SS.empty (successors cfg id)
      in
      let new_out =
        if id = cfg.exit then SS.union new_out (SS.singleton prog.output_var)
        else new_out
      in
      let new_in = transfer_live n.code new_out in
      if not (SS.equal new_in n.ann.df_in) || not (SS.equal new_out n.ann.df_out) then begin
        Hashtbl.replace cfg.nodes id { n with ann = { df_in = new_in; df_out = new_out } };
        changed := true
      end
    ) (sorted_ids cfg)
  done;
  cfg

(** Reaching Definitions *)

type def_site = {
  def_id : int; (* unique definition id *)
  def_var : string; (* variable being defined *)
  def_node : int; (* CFG node id *)
  def_idx : int; (* statement index within the block *)
}

let index_definitions (input_var : string) (g : plain_cfg) : def_site list * (string, IS.t) Hashtbl.t =
  let all_defs = ref [] in
  let var_to_defs : (string, IS.t) Hashtbl.t = Hashtbl.create 16 in
  let counter = ref 1 in
  let input_def = { def_id = 0; def_var = input_var; def_node = -1; def_idx = -1 } in
  all_defs := [input_def];
  Hashtbl.add var_to_defs input_var (IS.singleton 0);

  List.iter (fun id ->
    let n = Hashtbl.find g.nodes id in
    List.iteri (fun idx s ->
      match s with
      | SAssign (x, _) -> let did = !counter in
                          incr counter;
                          let d = { def_id = did; def_var = x; def_node = id; def_idx = idx } in
                          all_defs := d :: !all_defs;
                          let prev = match Hashtbl.find_opt var_to_defs x with
                            | Some s -> s 
                            | None -> IS.empty in
                            Hashtbl.replace var_to_defs x (IS.add did prev)
      | _ -> ()
    ) n.code
  ) (sorted_ids g);
  (List.rev !all_defs, var_to_defs)

(* Gen set for a block *)
let gen_reach (code : block) (node_id : int) (all_defs : def_site list) : IS.t =
  let last : (string, int) Hashtbl.t = Hashtbl.create 4 in
  List.iteri (fun idx s ->
    match s with
    | SAssign (x, _) ->
        (match List.find_opt (fun d -> d.def_node = node_id && d.def_idx = idx) all_defs with
         | Some d -> Hashtbl.replace last x d.def_id
         | None -> ())
    | _ -> ()
  ) code;
  Hashtbl.fold (fun _ did acc -> IS.add did acc) last IS.empty

(* Kill set for a block *)
let kill_reach (code : block) (node_id : int) (all_defs : def_site list) (var_to_defs : (string, IS.t) Hashtbl.t) : IS.t =
  let gen = gen_reach code node_id all_defs in
  let assigned_vars = List.fold_left (fun acc s -> SS.union acc (assigned_stmt s)) SS.empty code in
  SS.fold (fun x acc -> let all_x = match Hashtbl.find_opt var_to_defs x with
                                    | Some s -> s 
                                    | None -> IS.empty in
                                    IS.union acc (IS.diff all_x gen)
  ) assigned_vars IS.empty

(* Transfer function *)
let transfer_reach (code : block) (node_id : int) (all_defs : def_site list) (var_to_defs : (string, IS.t) Hashtbl.t) (df_in : IS.t) : IS.t =
  let gen  = gen_reach  code node_id all_defs in
  let kill = kill_reach code node_id all_defs var_to_defs in
  IS.union gen (IS.diff df_in kill)

(* Reaching-definitions analysis *)
let analyse_reaching (prog : program) (g : plain_cfg) : IS.t df_cfg * def_site list * (string, IS.t) Hashtbl.t =
  let (all_defs, var_to_defs) = index_definitions prog.input_var g in
  let ann0 = { df_in = IS.empty; df_out = IS.empty } in
  let cfg : IS.t df_cfg = annotate_cfg ann0 g in
  (match Hashtbl.find_opt cfg.nodes cfg.entry with
   | None -> ()
   | Some n -> Hashtbl.replace cfg.nodes cfg.entry { n with ann = { n.ann with df_in = IS.singleton 0 } });
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (
      fun id -> let n = Hashtbl.find cfg.nodes id in
                let new_in = 
                  if id = cfg.entry then n.ann.df_in
                  else List.fold_left (fun acc pid -> let pn = Hashtbl.find cfg.nodes pid in 
                                      IS.union acc pn.ann.df_out) IS.empty (predecessors cfg id)
      in
      let new_out = transfer_reach n.code id all_defs var_to_defs new_in in
      if not (IS.equal new_in n.ann.df_in) || not (IS.equal new_out n.ann.df_out) then begin
        Hashtbl.replace cfg.nodes id { n with ann = { df_in = new_in; df_out = new_out } };
        changed := true
      end
    ) (sorted_ids cfg)
  done;
  (cfg, all_defs, var_to_defs)

(** Pretty-printers *)

let pp_ss (s : SS.t) : string =
  if SS.is_empty s then "{}"
  else "{" ^ String.concat ", " (SS.elements s) ^ "}"

let pp_is (s : IS.t) : string =
  if IS.is_empty s then "{}"
  else "{" ^ String.concat ", " (List.map string_of_int (IS.elements s)) ^ "}"

let pp_df_ann_ss (ann : SS.t df_ann) : string =
  Printf.sprintf "  in=%s  out=%s" (pp_ss ann.df_in) (pp_ss ann.df_out)

let pp_df_ann_is (ann : IS.t df_ann) : string =
  Printf.sprintf "  in=%s  out=%s" (pp_is ann.df_in) (pp_is ann.df_out)

let pp_undef_warning (node_id : int) (var : string) : string =
  Printf.sprintf "Warning: variable '%s' may be used before being defined (in block %d)" var node_id

let pp_reach_ids_verbose (ids : IS.t) (all_defs : def_site list) : string =
  if IS.is_empty ids then "{}"
  else begin
    let elems = IS.elements ids in
    let strs = List.map (fun did ->
      match List.find_opt (fun d -> d.def_id = did) all_defs with
      | None -> string_of_int did
      | Some d ->
          if did = 0 then Printf.sprintf "[%d]%s=input" did d.def_var
          else Printf.sprintf "[%d]%s@n%d" did d.def_var d.def_node
    ) elems in
    "{" ^ String.concat ", " strs ^ "}"
  end

let pp_reach_ids (ids : IS.t) : string =
  if IS.is_empty ids then "{}"
  else "{" ^ String.concat ", " (List.map string_of_int (IS.elements ids)) ^ "}"

let pp_df_ann_reach (ann : IS.t df_ann) : string =
  Printf.sprintf "in=%s  out=%s" (pp_reach_ids ann.df_in) (pp_reach_ids ann.df_out)

let pp_block_reach ?(show_ids = true) (node_id : int) (code : block) (all_defs : def_site list) : string =
  List.mapi (fun idx s ->
    match s with
    | SAssign (x, e) ->
        let prefix =
          if show_ids then
            match List.find_opt (fun d -> d.def_node = node_id && d.def_idx = idx) all_defs with
            | Some d -> Printf.sprintf "[%d] " d.def_id
            | None -> ""
          else ""
        in
        prefix ^ pp_stmt s
    | _ -> pp_stmt s
  ) code
  |> String.concat ";\n"