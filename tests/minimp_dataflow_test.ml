(** MiniImp — Test Suite *)

open Minimp_ast
open Minimp_cfg
open Minimp_eval
open Minimp_dataflow
open Minimp_cfg_dot

(** Test runner *)

let total = ref 0
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

let check_raises name f =
  incr total;
  let raised = try ignore (f ()); false with _ -> true in
  if raised then begin incr passed; Printf.printf "  [PASS] %s\n" name end
  else begin incr failed; Printf.printf "  [FAIL] %s (expected exception)\n" name end

let section s =
  Printf.printf "\n=== %s ===\n" s

let summary () =
  Printf.printf "\n--- Results: %d/%d passed" !passed !total;
  if !failed > 0 then Printf.printf ", %d FAILED" !failed;
  Printf.printf " ---\n";
  if !failed > 0 then exit 1

(* Parse a MiniImp program string *)
let parse src =
  let lexbuf = Lexing.from_string src in
  Minimp_parser.program Minimp_lexer.token lexbuf

(* Run a program string with a given input *)
let run src input =
  let prog = parse src in
  eval_program prog input

(** 5. Data-Flow Analysis tests *)
let test_dataflow () =

  (* ================= Defined Variables ================= *)
  section "  Defined Variables";

  (* Straight-line: x := inp ; out := x + 1
     All three variables must be defined at the exit; no warnings. *)
  let prog = parse "def main with input inp output out as x := inp ; out := x + 1" in
  let g = cfg_of_program prog in
  let (cfg, warns) = analyse_defined prog g in
  let exit_ann = (Hashtbl.find cfg.nodes cfg.exit).ann in

  check_bool "defined: inp at exit" (SS.mem "inp" exit_ann.df_out) true;
  check_bool "defined: x at exit"   (SS.mem "x"   exit_ann.df_out) true;
  check_bool "defined: out at exit" (SS.mem "out" exit_ann.df_out) true;
  check_bool "defined: no warnings" (warns = []) true;

  (* If-then-else: out is assigned on both branches → defined at exit. *)
  let prog = parse "def main with input inp output out as if inp < 0 then out := 0 else out := inp" in
  let g = cfg_of_program prog in
  let (cfg, warns) = analyse_defined prog g in
  let exit_ann = (Hashtbl.find cfg.nodes cfg.exit).ann in

  check_bool "if: out defined at exit" (SS.mem "out" exit_ann.df_out) true;
  check_bool "if: no warnings"         (warns = []) true;

  (* While loop: out is assigned before the loop → defined at exit; no warnings. *)
  let prog = parse {|def main with input inp output out as
    out := 0 ;
    while inp < 10 do ( inp := inp + 1 ; out := out + 1 )|} in
  let g = cfg_of_program prog in
  let (cfg, warns) = analyse_defined prog g in
  let exit_ann = (Hashtbl.find cfg.nodes cfg.exit).ann in

  check_bool "while: out defined at exit" (SS.mem "out" exit_ann.df_out) true;
  check_bool "while: no warnings"         (warns = []) true;

  (* Possibly-undefined variable: x only assigned on one branch.
     A warning for 'x' must be emitted; inp must not trigger a warning. *)
  let prog = parse "def main with input inp output out as if inp < 0 then x := 1 else skip ; out := x" in
  let g = cfg_of_program prog in
  let (_cfg, warns) = analyse_defined prog g in

  let has_x_warn  = List.exists (fun (_, v) -> v = "x")   warns in
  let has_inp_warn = List.exists (fun (_, v) -> v = "inp") warns in
  check_bool "undef: warning for x"      has_x_warn   true;
  check_bool "undef: no warning for inp" has_inp_warn false;

  (* Redefinition: x is assigned twice; it is still defined at exit. *)
  let prog = parse "def main with input inp output out as x := 1 ; x := 2 ; out := x" in
  let g = cfg_of_program prog in
  let (cfg, warns) = analyse_defined prog g in
  let exit_ann = (Hashtbl.find cfg.nodes cfg.exit).ann in

  check_bool "redef: x defined at exit"   (SS.mem "x"   exit_ann.df_out) true;
  check_bool "redef: out defined at exit" (SS.mem "out" exit_ann.df_out) true;
  check_bool "redef: no warnings"         (warns = []) true;

  (* Dead store: b is assigned but never used; no undefined variable. *)
  let prog = parse {|def main with input inp output out as
    a := 3 ;
    b := a + inp ;
    out := inp|} in
  let g = cfg_of_program prog in
  let (cfg, warns) = analyse_defined prog g in
  let exit_ann = (Hashtbl.find cfg.nodes cfg.exit).ann in

  check_bool "dead: out defined at exit" (SS.mem "out" exit_ann.df_out) true;
  check_bool "dead: no warnings"         (warns = []) true;

  (* ================= Live Variables ================= *)
  section "  Live Variables";

  (* Straight-line: inp is needed immediately → live at entry.
     out is only written, never read before its definition → not live at entry. *)
  let prog = parse "def main with input inp output out as x := inp ; out := x + 1" in
  let g = cfg_of_program prog in
  let cfg = analyse_live prog g in
  let entry_ann = (Hashtbl.find cfg.nodes cfg.entry).ann in

  check_bool "live: inp at entry"     (SS.mem "inp" entry_ann.df_in) true;
  check_bool "live: out not at entry" (SS.mem "out" entry_ann.df_in) false;

  (* If-then-else: inp is used in the guard → live at entry. *)
  let prog = parse "def main with input inp output out as if inp < 0 then out := 0 else out := inp" in
  let g = cfg_of_program prog in
  let cfg = analyse_live prog g in
  let entry_ann = (Hashtbl.find cfg.nodes cfg.entry).ann in

  check_bool "if: inp live at entry" (SS.mem "inp" entry_ann.df_in) true;

  (* While loop: inp is tested in the guard → live at entry. *)
  let prog = parse {|def main with input inp output out as
    out := 0 ;
    while inp < 10 do ( inp := inp + 1 ; out := out + 1 )|} in
  let g = cfg_of_program prog in
  let cfg = analyse_live prog g in
  let entry_ann = (Hashtbl.find cfg.nodes cfg.entry).ann in

  check_bool "while: inp live at entry" (SS.mem "inp" entry_ann.df_in) true;

  (* Dead store: b is assigned but never read → not live at entry.
     inp is read by out := inp → live at entry.
     out is the output variable → live at exit. *)
  let prog = parse {|def main with input inp output out as
    a := 3 ;
    b := a + inp ;
    out := inp|} in
  let g = cfg_of_program prog in
  let cfg = analyse_live prog g in
  let entry_ann = (Hashtbl.find cfg.nodes cfg.entry).ann in
  let exit_ann  = (Hashtbl.find cfg.nodes cfg.exit).ann  in

  check_bool "dead: inp live at entry"   (SS.mem "inp" entry_ann.df_in) true;
  check_bool "dead: b not live at entry" (SS.mem "b"   entry_ann.df_in) false;
  check_bool "dead: out live at exit"    (SS.mem "out" exit_ann.df_out)  true;

  (* ================= Reaching Definitions ================= *)
  section "  Reaching Definitions";

  (* The virtual input definition (id = 0) must reach the entry block. *)
  let prog = parse "def main with input inp output out as x := inp ; out := x + 1" in
  let g = cfg_of_program prog in
  let (cfg, _all_defs, _) = analyse_reaching prog g in
  let entry_ann = (Hashtbl.find cfg.nodes cfg.entry).ann in

  check_bool "reaching: virtual input def reaches entry" (IS.mem 0 entry_ann.df_in) true;

  (* Redefinition: x := 1 is shadowed by x := 2; only the second def reaches exit. *)
  let prog = parse "def main with input inp output out as x := 1 ; x := 2 ; out := x" in
  let g = cfg_of_program prog in
  let (cfg, all_defs, _) = analyse_reaching prog g in
  let exit_ann = (Hashtbl.find cfg.nodes cfg.exit).ann in

  let defs_of_x = List.filter (fun d -> d.def_var = "x") all_defs in
  let reaching  = List.filter (fun d -> IS.mem d.def_id exit_ann.df_out) defs_of_x in
  check_bool "reaching: only 1 def of x reaches exit" (List.length reaching = 1) true;

  (* If-then-else: both branches assign out → both defs must reach the exit. *)
  let prog = parse "def main with input inp output out as if inp < 0 then out := 0 else out := inp" in
  let g = cfg_of_program prog in
  let (cfg, all_defs, _) = analyse_reaching prog g in
  let exit_ann = (Hashtbl.find cfg.nodes cfg.exit).ann in

  let defs_of_out = List.filter (fun d -> d.def_var = "out") all_defs in
  let n_reaching  = List.length
    (List.filter (fun d -> IS.mem d.def_id exit_ann.df_out) defs_of_out) in
  check_bool "reaching: both out-defs reach exit after if" (n_reaching = 2) true;

  (* While loop: body defs flow back around the back-edge.
     The initial out := 0 definition must be visible at the guard. *)
  let prog = parse {|def main with input inp output out as
    out := 0 ;
    while inp < 10 do ( inp := inp + 1 ; out := out + 1 )|} in
  let g = cfg_of_program prog in
  let (cfg, all_defs, _) = analyse_reaching prog g in
  let guard_node_id =
    Hashtbl.fold (fun id n acc ->
      match n.next with Branch _ -> id :: acc | _ -> acc
    ) cfg.nodes []
    |> (function [] -> -1 | x :: _ -> x)
  in
  check_bool "reaching: guard node found" (guard_node_id >= 0) true;
  if guard_node_id >= 0 then begin
    let guard_ann = (Hashtbl.find cfg.nodes guard_node_id).ann in
    let def_out0  = List.find_opt (fun d -> d.def_var = "out") all_defs in
    check_bool "reaching: initial out:=0 reaches guard"
      (match def_out0 with
       | Some d -> IS.mem d.def_id guard_ann.df_in
       | None   -> false) true
  end

let () =
  Printf.printf "Data-Flow Analysis Tests\n";
  test_dataflow ();
  summary ()
