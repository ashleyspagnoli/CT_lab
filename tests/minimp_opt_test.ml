(** MiniImp — Optimiser Test Suite *)

open Minimp_ast
open Minimp_cfg
open Minimp_cfg_dot
open Minimp_dataflow
open Minimp_opt
open Minimp_test_common

(* Collect every SAssign target in the CFG (across all blocks) *)
let assigned_vars_in_cfg (g : plain_cfg) : SS.t =
  Hashtbl.fold (fun _id n acc ->
    List.fold_left (fun acc s ->
      match s with
      | SAssign (x, _) -> SS.add x acc
      | _ -> acc
    ) acc n.code
  ) g.nodes SS.empty

(* True when the given variable is actually used *)
let var_used_in_cfg (x : string) (g : plain_cfg) : bool =
  Hashtbl.fold (fun _id n found ->
    found || List.exists (fun s -> SS.mem x (used_stmt s)) n.code
  ) g.nodes false

(* Count how many SAssign statements for variable [x] remain in the CFG *)
let count_assigns (x : string) (g : plain_cfg) : int =
  Hashtbl.fold (fun _id n acc ->
    acc + List.length (List.filter (function
      | SAssign (v, _) -> v = x
      | _ -> false) n.code)
  ) g.nodes 0

(* True when expression [e] appears as the RHS of some SAssign in the CFG *)
let rhs_exists (pred : expr -> bool) (g : plain_cfg) : bool =
  Hashtbl.fold (fun _id n found ->
    found || List.exists (function
      | SAssign (_, e) -> pred e
      | _ -> false) n.code
  ) g.nodes false

(* True when some SGuard condition satisfies [pred] in the CFG *)
let guard_exists (pred : bexpr -> bool) (g : plain_cfg) : bool =
  Hashtbl.fold (fun _id n found ->
    found || List.exists (function
      | SGuard b -> pred b
      | _ -> false) n.code
  ) g.nodes false

(** Dead Store Elimination *)
let test_dead_store_elimination () =

  section "Dead Store Elimination";

  (* A dead assignment that is never read is removed *)
  let prog = parse {|def main with input inp output out as
    b := inp + 1 ;
    out := inp|} in
    let g = cfg_of_program prog in
    let g' = eliminate_dead_stores prog g in
  check_bool "dse: dead var 'b' removed"
    (SS.mem "b" (assigned_vars_in_cfg g')) false;

  (* A variable that is used afterwards must be kept *)
  let prog = parse {|def main with input inp output out as
    x := inp + 1 ;
    out := x|} in
  let g = cfg_of_program prog in
  let g' = eliminate_dead_stores prog g in
  check_bool "dse: live var 'x' kept"
    (SS.mem "x" (assigned_vars_in_cfg g')) true;

  (* Sequential dead stores: first assignment to x is dead *)
  let prog = parse {|def main with input inp output out as
    x := 1 ;
    x := 2 ;
    out := x|} in
  let g = cfg_of_program prog in
  let g' = eliminate_dead_stores prog g in
  check_bool "dse: redundant first assign to 'x' eliminated"
    (count_assigns "x" g' = 1) true;

  (* Dead store inside an if-branch is removed *)
  let prog = parse {|def main with input inp output out as
    if inp < 0 then unused := 99 else skip ;
    out := inp|} in
  let g = cfg_of_program prog in
  let g' = eliminate_dead_stores prog g in
  check_bool "dse: 'unused' dead in if-branch eliminated"
    (SS.mem "unused" (assigned_vars_in_cfg g')) false;

  (* Dead store in loop body is removed when result never escapes *)
  let prog = parse {|def main with input inp output out as
    out := 0 ;
    while inp < 10 do (
      tmp := 42 ;
      inp := inp + 1
    )|} in
  let g = cfg_of_program prog in
  let g' = eliminate_dead_stores prog g in
  check_bool "dse: 'tmp' dead in while body eliminated"
    (SS.mem "tmp" (assigned_vars_in_cfg g')) false;

  (* Complex program with conditionals and multiple assignments *)
  let prog = parse {|def main with input inp output out as
    a := 0 ;
    a := 3 ;
    b := 2 ;
    x := 0 ;
    if 0 < inp then c := a + inp else a := 8 + b ;
    c := 2 + a ;
    out := 2 * c + b|} in
  let g = cfg_of_program prog in
  let g' = eliminate_dead_stores prog g in
  check_bool "dse: complex program preserves live vars"
    (SS.mem "out" (assigned_vars_in_cfg g')) true;
  check_bool "dse: 'x' is dead and eliminated"
    (SS.mem "x" (assigned_vars_in_cfg g')) false;
  check_bool "dse: first dead 'a' assignment eliminated"
    (count_assigns "a" g' = 2) true

(** Constant Folding *)
let test_constant_folding () =

  section "Constant Folding";

  (* Numeric BinOp with two literal operands is folded *)
  let prog = parse 
    "def main with input inp output out as out := 3 + 4" in
  let g = cfg_of_program prog in
  let g' = constant_folding g in
  check_bool "cf: 3+4 folded to Num 7"
    (rhs_exists (function Num 7 -> true | _ -> false) g') true;
  check_bool "cf: no BinOp remains after folding 3+4"
    (rhs_exists (function BinOp _ -> true | _ -> false) g') false;

  (* x-x is folded to 0 *)
  let prog = parse 
    "def main with input inp output out as out := inp - inp" in
  let g = cfg_of_program prog in
  let g' = constant_folding g in
  check_bool "cf: x-x folded to Num 0"
    (rhs_exists (function Num 0 -> true | _ -> false) g') true;

  (* 0*x is folded to 0 *)
  let prog = parse 
    "def main with input inp output out as out := 0 * inp" in
  let g = cfg_of_program prog in
  let g' = constant_folding g in
  check_bool "cf: 0*x folded to Num 0"
    (rhs_exists (function Num 0 -> true | _ -> false) g') true;

  (* x*0 is folded to 0 *)
  let prog = parse 
    "def main with input inp output out as out := inp * 0" in
  let g = cfg_of_program prog in
  let g' = constant_folding g in
  check_bool "cf: x*0 folded to Num 0"
    (rhs_exists (function Num 0 -> true | _ -> false) g') true;

  (* 1*x is simplified to x *)
  let prog = parse 
    "def main with input inp output out as out := 1 * inp" in
  let g = cfg_of_program prog in
  let g' = constant_folding g in
  check_bool "cf: 1*x simplified to Var"
    (rhs_exists (function Var "inp" -> true | _ -> false) g') true;

  (* x+0 is simplified to x *)
  let prog = parse 
    "def main with input inp output out as out := inp + 0" in
  let g = cfg_of_program prog in
  let g' = constant_folding g in
  check_bool "cf: x+0 simplified to Var"
    (rhs_exists (function Var "inp" -> true | _ -> false) g') true;

  (* Boolean guard with two constants is folded *)
  let prog = parse 
    "def main with input inp output out as if 2 < 5 then out := 1 else out := 0" in
  let g = cfg_of_program prog in
  let g' = constant_folding g in
  check_bool "cf: guard '2<5' folded to BoolLit true"
    (guard_exists (function BoolLit true -> true | _ -> false) g') true;

  (* Nested BinOp folded in one pass *)
  let prog = parse 
    "def main with input inp output out as out := (2 + 3) * (4 - 1)" in
  let g = cfg_of_program prog in
  let g' = constant_folding g in
  check_bool "cf: nested (2+3)*(4-1) folded to Num 15"
    (rhs_exists (function Num 15 -> true | _ -> false) g') true

(** Constant Propagation *)
let test_constant_propagation () =

  section "Constant Propagation";

  (* A literal assigned to x propagates into the next use *)
  let prog = parse {|def main with input inp output out as
    x := 5 ;
    out := x + inp|} 
  in
  let g = cfg_of_program prog in
  let g' = constant_propagation prog g in
  check_bool "cp: 'x' replaced by Num 5"
    (rhs_exists (function
      | BinOp (Num 5, Add, Var "inp") -> true
      | _ -> false) g') true;

  (* Chain propagation *)
  let prog = parse {|def main with input inp output out as
    x := 7 ;
    y := x ;
    out := y|} 
  in
  let g = cfg_of_program prog in
  let g' = constant_propagation prog g in
  check_bool "cp: chain x:=7;y:=x;out:=y → out := 7"
    (rhs_exists (function Num 7 -> true | _ -> false) g') true;

  (* Variable defined on both branches with same value is propagated *)
  let prog = parse {|def main with input inp output out as
    if inp < 0 then x := 3 else x := 3 ;
    out := x|} 
  in
  let g = cfg_of_program prog in
  let g' = constant_propagation prog g in
  check_bool "cp: x:=3 on both branches propagated to Num 3"
    (rhs_exists (function Num 3 -> true | _ -> false) g') true


(** Optimisation Pipeline *)
let test_pipeline () =

  section "Optimisation Pipeline";

  (* Propagation + folding + dead-store in sequence *)
  let prog = parse {|def main with input inp output out as
    x := 10 ;
    y := x + 5 ;
    out := y|} 
  in
  let g = cfg_of_program prog in
  let g' = optimise prog g in
  check_bool "pipeline: out := y propagated and folded to Num 15"
    (rhs_exists (function Num 15 -> true | _ -> false) g') true;

  (* Pipeline is idempotent: running it twice gives the same CFG *)
  let prog = parse {|def main with input inp output out as
    x := 2 + 3 ;
    y := x * 1 ;
    out := y + 0|} 
  in
  let g  = cfg_of_program prog in
  let g1 = optimise prog g in
  let g2 = optimise prog g1 in
  check_bool "pipeline is idempotent"
    (cfg_fingerprint g1 = cfg_fingerprint g2) true;

  (* No undefined-variable warnings remain after pipeline *)
  let prog = parse {|def main with input inp output out as
    k := 4 ;
    out := inp * k|} 
  in
  let g = cfg_of_program prog in
  let g' = optimise prog g in
  let warns = check_undefined prog g' in
  check_bool "pipeline: no new undefined-variable warnings after optimisation"
    (warns = []) true;
    
  (* Propagation, constant folding and dead-store elimination *)
  let prog = parse {|def main with input inp output out as
    a := 1 ;
    b := 10 - (a * 1) ;
    c := b * 2 ;
    if (c < 10) then c := c - 10 else skip ;
    out := c * (2 * a)|} 
  in
  let g = cfg_of_program prog in
  let g' = optimise prog g in
  check_bool "pipeline: dead store 'a' eliminated"
    (SS.mem "a" (assigned_vars_in_cfg g')) false;
  check_bool "pipeline: dead store 'b' eliminated"
    (SS.mem "b" (assigned_vars_in_cfg g')) false

(** Entry point *)
let () =
  Printf.printf "Optimiser Tests\n";
  test_dead_store_elimination ();
  test_constant_folding ();
  test_constant_propagation ();
  test_pipeline ();
  summary ()
