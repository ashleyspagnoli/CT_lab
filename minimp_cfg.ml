(** MiniImp — Control-Flow Graph *)

open Minimp_ast

type stmt =
  | SSkip (* skip *)
  | SAssign of string * expr (* <var> := <e> *)
  | SGuard of bexpr (* <b>? *)

(* Basic blocks, no jumps *)

type block = stmt list

(* Successor relation *)

type next =
  | End (* No successor, terminal node *)
  | Next of int (* Unconditional successor *)
  | Branch of int * int (* true branch id, false branch id *)

(* CFG node *)

type node = {
  id : int;
  code : block;
  next : next;
}

(* CFG full graph *)

type cfg = {
  nodes : (int, node) Hashtbl.t;
  entry : int;
  exit : int;
}

(* Pretty-printer *)

let pp_op = function Add -> "+" | Sub -> "-" | Mul -> "*"

let rec pp_expr = function
  | Num n -> string_of_int n
  | Var x -> x
  | BinOp(l,op,r) -> Printf.sprintf "(%s %s %s)" (pp_expr l) (pp_op op) (pp_expr r)

let rec pp_bexpr = function
  | BoolLit b -> string_of_bool b
  | BoolAnd(a,b) -> Printf.sprintf "(%s and %s)" (pp_bexpr a) (pp_bexpr b)
  | BoolNot b -> Printf.sprintf "(not %s)" (pp_bexpr b)
  | BoolLt(e1,e2) -> Printf.sprintf "%s < %s" (pp_expr e1) (pp_expr e2)

let pp_stmt = function
  | SSkip -> "skip"
  | SAssign(x,e) -> Printf.sprintf "%s := %s" x (pp_expr e)
  | SGuard b -> Printf.sprintf "%s?" (pp_bexpr b)

let pp_block stmts = String.concat "; " (List.map pp_stmt stmts)

let pp_next = function
  | End -> "⊥"
  | Next id -> string_of_int id
  | Branch(t, f) -> Printf.sprintf "true→%d, false→%d" t f

let pp_cfg (g : cfg) : string =
  let buf = Buffer.create 256 in
  Buffer.add_string buf
    (Printf.sprintf "CFG  entry=%d  exit=%d\n" g.entry g.exit);
  let ids =
    Hashtbl.fold (fun id _ acc -> id :: acc) g.nodes []
    |> List.sort compare
  in
  List.iter (fun id ->
    let n = Hashtbl.find g.nodes id in
    Buffer.add_string buf
      (Printf.sprintf "  [%d]  code: %-40s  next: %s\n"
         id (pp_block n.code) (pp_next n.next))
  ) ids;
  Buffer.contents buf

(** CFG builder *)

(* Fresh-id generator *)
let next_id =
  let ctr = ref 0 in
  fun () -> incr ctr; !ctr

(* Allocate a new node *)
let make_node (nodes : (int, node) Hashtbl.t) (code : block) (next : next) : int =
  let id = next_id () in
  Hashtbl.add nodes id { id; code; next };
  id

(* Next field of an existing node *)
let set_next (nodes : (int, node) Hashtbl.t) (id : int) (nxt : next) : unit =
  match Hashtbl.find_opt nodes id with
  | None -> failwith (Printf.sprintf "set_next: node %d not found" id)
  | Some n -> Hashtbl.replace nodes id { n with next = nxt }

(* Plain node = if it carries no guard and has no successor *)
let is_plain (n : node) : bool =
  n.next = End && not (List.exists(function SGuard _ -> true | _ -> false) n.code)

(* Replace every occurrence of old_id with new_id in all edges *)
let redirect_all (nodes : (int, node) Hashtbl.t) (old_id : int) (new_id : int) : unit =
  let replace_next = function
    | Next x when x = old_id -> Next new_id
    | Branch (t, f) ->
        Branch ((if t = old_id then new_id else t), (if f = old_id then new_id else f))
    | other -> other
  in
  Hashtbl.iter (fun id n ->
    let nxt' = replace_next n.next in
    if nxt' <> n.next then
      Hashtbl.replace nodes id { n with next = nxt' }
  ) nodes

(** Commands Control Flow Graph *)

let rec cfg_of_cmd (nodes : (int, node) Hashtbl.t) (c : cmd)
    : int * int =
  match c with
  | Skip ->
      let id = make_node nodes [SSkip] End in
      (id, id)
  | Assign (x, e) ->
      let id = make_node nodes [SAssign (x, e)] End in
      (id, id)
  | Seq (c1, c2) ->
      let (i1, f1) = cfg_of_cmd nodes c1 in
      let (i2, f2) = cfg_of_cmd nodes c2 in
      merge_seq nodes i1 f1 i2 f2
  | If (b, c_then, c_else) ->
      let (i_then, f_then) = cfg_of_cmd nodes c_then in
      let (i_else, f_else) = cfg_of_cmd nodes c_else in
      let join = make_node nodes [SSkip] End in
      set_next nodes f_then (Next join);
      set_next nodes f_else (Next join);
      let collapse_skip_bridge fid iid =
        let n = Hashtbl.find nodes fid in
        if n.code = [SSkip] && n.next = Next join then begin
          redirect_all nodes fid join;
          Hashtbl.remove nodes fid;
          if iid = fid then join else iid
        end else
          iid
      in
      let real_i_then = collapse_skip_bridge f_then i_then in
      let real_i_else = collapse_skip_bridge f_else i_else in
      let guard = make_node nodes [SGuard b] (Branch (real_i_then, real_i_else)) in
      (guard, join)
  | While (b, body) ->
      let (i_body, f_body) = cfg_of_cmd nodes body in
      let f = make_node nodes [SSkip] End in
      let guard = make_node nodes [SGuard b] (Branch (i_body, f)) in
      set_next nodes f_body (Next guard);
      (guard, f)

(* Merge the sequential composition c1;c2 given their entry/exit ids *)
and merge_seq nodes i1 f1 i2 f2 =
  let n_f1 = Hashtbl.find nodes f1 in
  let n_i2 = Hashtbl.find nodes i2 in
  let clean_code l = List.filter (function SSkip -> false | _ -> true) l in (* Clean SSkip *)
  if is_plain n_f1 && is_plain n_i2 then begin
    let merged_code = clean_code (n_f1.code @ n_i2.code) in
    Hashtbl.replace nodes f1 { 
      n_f1 with
      code = (if merged_code = [] then [SSkip] else merged_code); 
      next = n_i2.next 
    };
    Hashtbl.remove nodes i2;
    let new_f2 = if f2 = i2 then f1 else f2 in
    (i1, new_f2)
  end else begin
    let f1_clean = clean_code n_f1.code in
    if f1_clean <> n_f1.code then
      Hashtbl.replace nodes f1 { n_f1 with code = (if f1_clean = [] then [SSkip] else f1_clean) };    
    set_next nodes f1 (Next i2);
    (i1, f2)
  end

let cfg_of_program (prog : program) : cfg =
  let nodes = Hashtbl.create 16 in
  let (entry, exit_) = cfg_of_cmd nodes prog.body in
  { nodes; entry; exit = exit_ }