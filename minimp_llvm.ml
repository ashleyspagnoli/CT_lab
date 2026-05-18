(** MiniImp — LLVM IR Code Generator *)

open Minimp_ast
open Minimp_cfg

(* Fresh name generator *)

let fresh_counter = ref 0

let reset_fresh () = fresh_counter := 0

let fresh (base : string) : string =
  let n = !fresh_counter in
  incr fresh_counter;
  Printf.sprintf "%%%s.%d" base n

(* Buffer helpers *)

let emit (buf : Buffer.t) (s : string) : unit =
  Buffer.add_string buf s;
  Buffer.add_char buf '\n'

(* Expression compilation *)

let rec compile_expr (buf : Buffer.t) (e : expr) : string =
  match e with
  | Num n -> string_of_int n
  | Var x ->
      let ptr = Printf.sprintf "%%%s.addr" x in
      let tmp = fresh x in
      emit buf (Printf.sprintf "  %s = load i64, ptr %s" tmp ptr);
      tmp
  | BinOp (l, op, r) ->
      let vl = compile_expr buf l in
      let vr = compile_expr buf r in
      let tmp = fresh "tmp" in
      let opname = match op with Add -> "add" | Sub -> "sub" | Mul -> "mul" in
      emit buf (Printf.sprintf "  %s = %s i64 %s, %s" tmp opname vl vr);
      tmp

(* Boolean expression compilation *)

let rec compile_bexpr (buf : Buffer.t) (b : bexpr) : string =
  match b with
  | BoolLit true  -> "1"
  | BoolLit false -> "0"
  | BoolNot b1 ->
      let v = compile_bexpr buf b1 in
      let tmp = fresh "not" in
      emit buf (Printf.sprintf "  %s = xor i1 %s, 1" tmp v);
      tmp
  | BoolAnd (b1, b2) ->
      let v1 = compile_bexpr buf b1 in
      let v2 = compile_bexpr buf b2 in
      let tmp = fresh "and" in
      emit buf (Printf.sprintf "  %s = and i1 %s, %s" tmp v1 v2);
      tmp
  | BoolLt (e1, e2) ->
      let v1 = compile_expr buf e1 in
      let v2 = compile_expr buf e2 in
      let tmp = fresh "cmp" in
      emit buf (Printf.sprintf "  %s = icmp slt i64 %s, %s" tmp v1 v2);
      tmp

(* Collect all variables appearing in the CFG *)

let collect_vars (g : plain_cfg) : string list =
  let vars : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  let add v = Hashtbl.replace vars v () in
  let rec scan_expr = function
    | Var v -> add v
    | BinOp (l, _, r) -> scan_expr l; scan_expr r
    | Num _ -> ()
  in
  let rec scan_bexpr = function
    | BoolLit _ -> ()
    | BoolNot b -> scan_bexpr b
    | BoolAnd (b1, b2) -> scan_bexpr b1; scan_bexpr b2
    | BoolLt (e1, e2) -> scan_expr e1; scan_expr e2
  in
  Hashtbl.iter (fun _ n ->
    List.iter (fun s ->
      match s with
      | SAssign (x, e) -> add x; scan_expr e
      | SGuard b -> scan_bexpr b
      | SSkip -> ()
    ) n.code
  ) g.nodes;
  List.sort String.compare
    (Hashtbl.fold (fun v () acc -> v :: acc) vars [])

(* Compile a single block, returning its IR as a string *)

let compile_block_full (node : unit node) : string =
  let buf = Buffer.create 128 in
  emit buf (Printf.sprintf "block%d:" node.id);
  (match node.next with
   | Branch (t_id, f_id) ->
       let guard_opt =
         List.find_opt (function SGuard _ -> true | _ -> false) node.code
       in
       (* Emit non-guard statements first *)
       List.iter (fun s ->
         match s with
         | SAssign (x, e) ->
             let v = compile_expr buf e in
             emit buf (Printf.sprintf "  store i64 %s, ptr %%%s.addr" v x)
         | SGuard _ | SSkip -> ()
       ) node.code;
       (* Compile the guard expression right before the branch *)
       let cond =
         match guard_opt with
         | Some (SGuard b) -> compile_bexpr buf b
         | _ -> "1"
       in
       emit buf
         (Printf.sprintf "  br i1 %s, label %%block%d, label %%block%d" cond t_id f_id)
   | _ ->
       List.iter (fun s ->
         match s with
         | SAssign (x, e) ->
             let v = compile_expr buf e in
             emit buf (Printf.sprintf "  store i64 %s, ptr %%%s.addr" v x)
         | SSkip | SGuard _ -> ()
       ) node.code;
       (match node.next with
        | End -> ()
        | Next tgt ->
            emit buf (Printf.sprintf "  br label %%block%d" tgt)
        | Branch _ -> assert false));
  Buffer.contents buf

(* Full module generation *)

let generate_llvm (prog : program) (g : plain_cfg) : string =
  reset_fresh ();
  let buf = Buffer.create 512 in
  emit buf (Printf.sprintf "define i64 @func(i64 %%%s) {" prog.input_var);
  emit buf "entry:";

  (* Collect all vars from the CFG *)
  let cfg_vars = collect_vars g in
  let all_vars =
    let s = Hashtbl.create 8 in
    List.iter (fun v -> Hashtbl.replace s v ()) cfg_vars;
    Hashtbl.replace s prog.input_var ();
    Hashtbl.replace s prog.output_var ();
    List.sort String.compare
      (Hashtbl.fold (fun v () acc -> v :: acc) s [])
  in

  (* Emit all alloca instructions in the entry block *)
  List.iter (fun v ->
    emit buf (Printf.sprintf "  %%%s.addr = alloca i64" v)
  ) all_vars;

  (* Store the function argument into its alloca slot *)
  emit buf (Printf.sprintf "  store i64 %%%s, ptr %%%s.addr"
              prog.input_var prog.input_var);
  (* Jump to the CFG entry block *)
  emit buf (Printf.sprintf "  br label %%block%d" g.entry);
  emit buf "";

  (* Emit all CFG blocks in sorted order *)
  let ids = sorted_ids g in
  List.iter (fun id ->
    let node = Hashtbl.find g.nodes id in
    if id = g.exit then begin
      (* Emit the block body *)
      Buffer.add_string buf (compile_block_full node);
      (* After the last statement, load the output var and return *)
      let out_reg = fresh prog.output_var in
      emit buf (Printf.sprintf "  %s = load i64, ptr %%%s.addr" out_reg prog.output_var);
      emit buf (Printf.sprintf "  ret i64 %s" out_reg);
      emit buf ""
    end else begin
      Buffer.add_string buf (compile_block_full node);
      emit buf ""
    end
  ) ids;

  emit buf "}";
  Buffer.contents buf

(* Write to file *)

let write_llvm_file (filename : string) (prog : program) (g : plain_cfg) : unit =
  let ir = generate_llvm prog g in
  let oc = open_out filename in
  output_string oc ir;
  close_out oc;
  Printf.printf "LLVM IR written to %s\n%!" filename