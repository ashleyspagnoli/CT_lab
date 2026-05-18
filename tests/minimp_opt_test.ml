(** MiniImp — Optimiser Test Suite *)

open Minimp_ast
open Minimp_cfg
open Minimp_dataflow
open Minimp_opt

(** ── Test runner (same style as the dataflow test suite) ── *)

let total  = ref 0
let passed = ref 0
let failed = ref 0

let check name result expected pp =
  incr total;
  if result = expected then begin
    incr passed;
    Printf.printf "  [PASS] %s\n" name
  end else begin
    incr failed;
    Printf.printf "  [FAIL] %s\n    expected: %s\n    got:      %s\n"
      name (pp expected) (pp result)
  end

let check_bool name result expected =
  check name result expected string_of_bool

let check_int name result expected =
  check name result expected string_of_int

let section s =
  Printf.printf "\n=== %s ===\n" s

let summary () =
  Printf.printf "\n--- Results: %d/%d passed" !passed !total;
  if !failed > 0 then Printf.printf ", %d FAILED" !failed;
  Printf.printf " ---\n";
  if !failed > 0 then exit 1

(** Parse a MiniImp program string. *)
let parse src =
  let lexbuf = Lexing.from_string src in
  Minimp_parser.program Minimp_lexer.token lexbuf

(** Collect every SAssign target in the CFG (across all blocks). *)
let assigned_vars_in_cfg (g : plain_cfg) : SS.t =
  Hashtbl.fold (fun _id n acc ->
    List.fold_left (fun acc s ->
      match s with
      | SAssign (x, _) -> SS.add x acc
      | _ -> acc
    ) acc n.code
  ) g.nodes SS.empty

(** True when the given variable appears on the RHS of at least one
    statement in the CFG (i.e. is actually used). *)
let var_used_in_cfg (x : string) (g : plain_cfg) : bool =
  Hashtbl.fold (fun _id n found ->
    found || List.exists (fun s -> SS.mem x (used_stmt s)) n.code
  ) g.nodes false

(** Count how many SAssign statements for variable [x] remain in the CFG. *)
let count_assigns (x : string) (g : plain_cfg) : int =
  Hashtbl.fold (fun _id n acc ->
    acc + List.length (List.filter (function
      | SAssign (v, _) -> v = x
      | _ -> false) n.code)
  ) g.nodes 0

(** True when expression [e] appears as the RHS of some SAssign in the CFG. *)
let rhs_exists (pred : expr -> bool) (g : plain_cfg) : bool =
  Hashtbl.fold (fun _id n found ->
    found || List.exists (function
      | SAssign (_, e) -> pred e
      | _ -> false) n.code
  ) g.nodes false

(** True when some SGuard condition satisfies [pred] in the CFG. *)
let guard_exists (pred : bexpr -> bool) (g : plain_cfg) : bool =
  Hashtbl.fold (fun _id n found ->
    found || List.exists (function
      | SGuard b -> pred b
      | _ -> false) n.code
  ) g.nodes false

(** Dead Store Elimination *)
let test_dead_store_elimination () =

  section "  Dead Store Elimination";

  (* A dead assignment that is never read is removed. *)
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

  (* Sequential dead stores: first assignment to x is dead. *)
  let prog = parse {|def main with input inp output out as
    x := 1 ;
    x := 2 ;
    out := x|} in
  let g = cfg_of_program prog in
  let g' = eliminate_dead_stores prog g in
  check_bool "dse: redundant first assign to 'x' eliminated"
    (count_assigns "x" g' = 1) true;

  (* The output variable is always live; its assignment is kept. *)
  let prog = parse {|def main with input inp output out as
    out := inp|} in
  let g = cfg_of_program prog in
  let g' = eliminate_dead_stores prog g in

  check_bool "dse: 'out' assignment preserved (output var is live)"
    (SS.mem "out" (assigned_vars_in_cfg g')) true;

  (* Dead store inside an if-branch is removed. *)
  let prog = parse {|def main with input inp output out as
    if inp < 0 then unused := 99 else skip ;
    out := inp|} in
  let g = cfg_of_program prog in
  let g' = eliminate_dead_stores prog g in
  check_bool "dse: 'unused' dead in if-branch eliminated"
    (SS.mem "unused" (assigned_vars_in_cfg g')) false;

  (* Dead store in loop body is removed when result never escapes. *)
  let prog = parse {|def main with input inp output out as
    out := 0 ;
    while inp < 10 do (
      tmp := 42 ;
      inp := inp + 1
    )|} in
  let g = cfg_of_program prog in
  let g' = eliminate_dead_stores prog g in
  check_bool "dse: 'tmp' dead in while body eliminated"
    (SS.mem "tmp" (assigned_vars_in_cfg g')) false

(** Constant Folding *)
let test_constant_folding () =

  section "  Constant Folding";

  (* Numeric BinOp with two literal operands is folded. *)
  let prog = parse {|def main with input inp output out as
    out := 3 + 4|} in
  let g = cfg_of_program prog in
  let g' = constant_folding g in
  check_bool "cf: 3+4 folded to Num 7"
    (rhs_exists (function Num 7 -> true | _ -> false) g') true;
  check_bool "cf: no BinOp remains after folding 3+4"
    (rhs_exists (function BinOp _ -> true | _ -> false) g') false;

  (* x - x is folded to 0. *)
  let prog = parse {|def main with input inp output out as
    out := inp - inp|} in
  let g = cfg_of_program prog in
  let g' = constant_folding g in
  check_bool "cf: x-x folded to Num 0"
    (rhs_exists (function Num 0 -> true | _ -> false) g') true;

  (* 0 * x is folded to 0 (left absorption) *)
  let prog = parse {|def main with input inp output out as
    out := 0 * inp|} in
  let g = cfg_of_program prog in
  let g' = constant_folding g in
  check_bool "cf: 0*x folded to Num 0"
    (rhs_exists (function Num 0 -> true | _ -> false) g') true;

  (* x * 0 is folded to 0 *)
  let prog = parse {|def main with input inp output out as
    out := inp * 0|} in
  let g = cfg_of_program prog in
  let g' = constant_folding g in
  check_bool "cf: x*0 folded to Num 0"
    (rhs_exists (function Num 0 -> true | _ -> false) g') true;

  (* 1 * x is simplified to x (identity). *)
  let prog = parse {|def main with input inp output out as
    out := 1 * inp|} in
  let g = cfg_of_program prog in
  let g' = constant_folding g in
  check_bool "cf: 1*x simplified to Var"
    (rhs_exists (function Var "inp" -> true | _ -> false) g') true;

  (* x + 0 is simplified to x. *)
  let prog = parse {|def main with input inp output out as
    out := inp + 0|} in
  let g = cfg_of_program prog in
  let g' = constant_folding g in
  check_bool "cf: x+0 simplified to Var"
    (rhs_exists (function Var "inp" -> true | _ -> false) g') true;

  (*Boolean guard with two constants is folded. *)
  let prog = parse {|def main with input inp output out as
    if 2 < 5 then out := 1 else out := 0|} in
  let g = cfg_of_program prog in
  let g' = constant_folding g in
  check_bool "cf: guard '2<5' folded to BoolLit true"
    (guard_exists (function BoolLit true -> true | _ -> false) g') true;

  (* Nested BinOp folded in one pass. *)
  let prog = parse {|def main with input inp output out as
    out := (2 + 3) * (4 - 1)|} in
  let g = cfg_of_program prog in
  let g' = constant_folding g in
  check_bool "cf: nested (2+3)*(4-1) folded to Num 15"
    (rhs_exists (function Num 15 -> true | _ -> false) g') true

(** Constant Propagation *)
let test_constant_propagation () =

  section "  Constant Propagation";

  (* A literal assigned to x propagates into the next use. *)
  let prog = parse {|def main with input inp output out as
    x := 5 ;
    out := x + inp|} in
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
    out := y|} in
  let g = cfg_of_program prog in
  let g' = constant_propagation prog g in
  check_bool "cp: chain x:=7;y:=x;out:=y → out := 7"
    (rhs_exists (function Num 7 -> true | _ -> false) g') true;

  (* Variable assigned on only ONE branch is NOT propagated after the join. *)
  let prog = parse {|def main with input inp output out as
    if inp < 0 then x := 1 else skip ;
    out := x|} in
  let g = cfg_of_program prog in
  let g' = constant_propagation prog g in
  check_bool "cp: x not propagated after branch with skip"
    (rhs_exists (function Var "x" -> true | _ -> false) g') true;

  (* Variable defined on both branches with SAME value IS propagated. *)
  let prog = parse {|def main with input inp output out as
    if inp < 0 then x := 3 else x := 3 ;
    out := x|} in
  let g = cfg_of_program prog in
  let g' = constant_propagation prog g in
  check_bool "cp: x:=3 on both branches propagated to Num 3"
    (rhs_exists (function Num 3 -> true | _ -> false) g') true;

  (* Input variable is NOT replaced (it has no known constant value). *)
  let prog = parse {|def main with input inp output out as
    out := inp|} in
  let g = cfg_of_program prog in
  let g' = constant_propagation prog g in
  check_bool "cp: input variable 'inp' not replaced by a constant"
    (rhs_exists (function Var "inp" -> true | _ -> false) g') true;

  (* Inside a while loop the loop variable is NOT propagated as a constant. *)
  let prog = parse {|def main with input inp output out as
    out := 0 ;
    while inp < 10 do ( inp := inp + 1 ; out := out + 1 )|} in
  let g = cfg_of_program prog in
  let g' = constant_propagation prog g in
  check_bool "cp: 'out' in loop body not replaced (multiple reaching defs)"
    (var_used_in_cfg "out" g') true

(** Optimisation Pipeline *)
let test_pipeline () =

  section "  Optimisation Pipeline";

  (* Constant folding followed by dead store elimination. *)
  let prog = parse {|def main with input inp output out as
    dead := 2 + 3 ;
    out := inp|} in
  let g = cfg_of_program prog in
  let g' = optimise prog g in
  check_bool "pipeline: dead literal assign eliminated"
    (SS.mem "dead" (assigned_vars_in_cfg g')) false;

  (* Propagation + folding + dead-store in sequence. *)
  let prog = parse {|def main with input inp output out as
    x := 10 ;
    y := x + 5 ;
    out := y|} in
  let g = cfg_of_program prog in
  let g' = optimise prog g in
  check_bool "pipeline: out := y propagated and folded to Num 15"
    (rhs_exists (function Num 15 -> true | _ -> false) g') true;

  (* Full pipeline on a compound program preserves semantics. *)
  let prog = parse {|def main with input inp output out as
    out := 0 ;
    while inp < 3 do ( inp := inp + 1 ; out := out + 1 )|} in
  let g = cfg_of_program prog in
  let g' = optimise prog g in
  check_bool "pipeline: 'out' still assigned after loop optimisation"
    (SS.mem "out" (assigned_vars_in_cfg g')) true;
  check_bool "pipeline: 'inp' still used in loop guard"
    (var_used_in_cfg "inp" g') true;

  (* Pipeline is idempotent: running it twice gives the same CFG *)
  let prog = parse {|def main with input inp output out as
    x := 2 + 3 ;
    y := x * 1 ;
    out := y + 0|} in
  let g  = cfg_of_program prog in
  let g1 = optimise prog g in
  let g2 = optimise prog g1 in
  check_bool "pipeline: second pass is idempotent (same fingerprint)"
    (cfg_fingerprint g1 = cfg_fingerprint g2) true;

  (* No undefined-variable warnings remain after pipeline. *)
  let prog = parse {|def main with input inp output out as
    k := 4 ;
    out := inp * k|} in
  let g = cfg_of_program prog in
  let g' = optimise prog g in
  let warns = check_undefined prog g' in
  check_bool "pipeline: no new undefined-variable warnings after optimisation"
    (warns = []) true

(** Entry point *)
let () =
  Printf.printf "Optimiser Tests\n";
  test_dead_store_elimination ();
  test_constant_folding ();
  test_constant_propagation ();
  test_pipeline ();
  summary ()

(* To print a graph: Printf.printf "%s\n" (pp_cfg g); *)