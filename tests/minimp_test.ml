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

(** Evaluator tests *)

let test_eval () =
  section "Evaluator";

  (* Arithmetic *)
  check_int "assign constant"
    (run "def main with input x output y as y := 42" 0) 42;

  check_int "use input"
    (run "def main with input x output y as y := x" 7) 7;

  check_int "addition"
    (run "def main with input x output y as y := x + 3" 4) 7;

  check_int "subtraction"
    (run "def main with input x output y as y := x - 1" 10) 9;

  check_int "multiplication"
    (run "def main with input x output y as y := x * x" 5) 25;

  check_int "nested binop"
    (run "def main with input x output y as y := (x + 1) * (x - 1)" 5) 24;

  (* Sequencing *)
  check_int "sequence"
    (run "def main with input x output y as y := x ; y := y + 1" 3) 4;

  (* If-then-else *)
  check_int "if true branch"
    (run "def main with input x output y as if x < 10 then y := 1 else y := 0" 5) 1;

  check_int "if false branch"
    (run "def main with input x output y as if x < 10 then y := 1 else y := 0" 15) 0;

  (* While *)
  check_int "while sum 1..n"
    (run {|def main with input n output s as
           s := 0 ;
           while (1 < n) do (
             s := s + n ;
             n := n - 1
           ) ;
           s := s + n|} 5) 15;

  check_int "while not entered"
    (run "def main with input x output y as y := 0 ; while (x < 0) do y := y + 1" 5) 0;

  check_int "factorial 5"
    (run {|def main with input n output r as
             r := 1 ;
             while (1 < n) do (
               r := r * n ;
               n := n - 1
             )|} 5) 120;

  (* Boolean expressions *)
  check_int "bool and — both true"
    (run "def main with input x output y as if x < 10 and 1 < x then y := 1 else y := 0" 5) 1;

  check_int "bool and — one false"
    (run "def main with input x output y as if x < 10 and 1 < x then y := 1 else y := 0" 0) 0;

  check_int "bool not"
    (run "def main with input x output y as if not (x < 5) then y := 1 else y := 0" 7) 1;

  (* Error: undefined variable *)
  check_raises "undefined variable raises"
    (fun () -> run "def main with input x output y as y := z" 0)

(** 2. Parser / Lexer tests *)

let test_parser () =
  section "Parser / Lexer";

  let prog = parse "def main with input x output y as y := x" in
  check "input var" prog.input_var "x" (fun s -> s);
  check "output var" prog.output_var "y" (fun s -> s);
  check "body is Assign" prog.body (Assign ("y", Var "x"))
    (fun _ -> "<cmd>");

  let prog2 = parse "def main with input x output y as if x < 1 then y := 0 else y := 1" in
  check "if body" prog2.body
    (If (BoolLt (Var "x", Num 1), Assign ("y", Num 0), Assign ("y", Num 1)))
    (fun _ -> "<cmd>");

  let prog3 = parse "def main with input x output y as while x < 1 do x := x + 1 ; y := x" in
  (match prog3.body with
   | Seq (While _, Assign _) -> incr total; incr passed; Printf.printf "  [PASS] while-seq structure\n"
   | _ -> incr total; incr failed; Printf.printf "  [FAIL] while-seq structure\n");

  check_raises "lexer error raises"
    (fun () -> parse "def main with input x output y as y := @");

  check_raises "parser error raises"
    (fun () -> parse "def main with input x output y as if then skip else skip")

(** 3. CFG builder tests *)

let cfg_of src = cfg_of_program (parse src)

(* Count nodes in a CFG *)
let node_count g = Hashtbl.length g.nodes

(* Check entry node has given code *)
let entry_code g = (Hashtbl.find g.nodes g.entry).code

(* Check that all node ids referenced in edges actually exist *)
let all_edges_valid g =
  Hashtbl.fold (fun _ n ok ->
    ok && match n.next with
    | End -> true
    | Next id -> Hashtbl.mem g.nodes id
    | Branch (t, f) -> Hashtbl.mem g.nodes t && Hashtbl.mem g.nodes f
  ) g.nodes true

let test_cfg () =
  section "CFG builder";

  let g_skip = cfg_of "def main with input x output y as skip" in
  check_int "skip: 1 node" (node_count g_skip) 1;
  check_bool "skip: entry=exit" (g_skip.entry = g_skip.exit) true;

  let g_assign = cfg_of "def main with input x output y as y := x" in
  check_int "assign: 1 node" (node_count g_assign) 1;
  check "assign: entry code" (entry_code g_assign) [SAssign ("y", Var "x")]
    (fun _ -> "<block>");

  (* Seq of two assigns should be merged into one block *)
  let g_seq = cfg_of "def main with input x output y as y := x ; y := y + 1" in
  check_int "seq: merged into 1 node" (node_count g_seq) 1;

  (* If: guard + two branches + join = 4 nodes (or 3 if branches are trivial) *)
  let g_if = cfg_of
    "def main with input x output y as if x < 5 then y := 1 else y := 0" in
  check_bool "if: entry is guard"
    (match entry_code g_if with [SGuard _] -> true | _ -> false) true;
  check_bool "if: all edges valid" (all_edges_valid g_if) true;

  (* While: guard -> body -> guard (back edge) *)
  let g_while = cfg_of
    "def main with input x output y as while x < 10 do x := x + 1" in
  check_bool "while: entry is guard"
    (match entry_code g_while with [SGuard _] -> true | _ -> false) true;
  check_bool "while: all edges valid" (all_edges_valid g_while) true;
  (* Body must loop back to guard *)
  let guard_id = g_while.entry in
  let body_id = match (Hashtbl.find g_while.nodes guard_id).next with
    | Branch (t, _) -> t | _ -> -1 in
  let body_next = (Hashtbl.find g_while.nodes body_id).next in
  check_bool "while: body loops back to guard"
    (body_next = Next guard_id) true;

  check_bool "complex: all edges valid"
    (all_edges_valid (cfg_of {|def main with input n output r as
       r := 1 ;
       while (1 < n) do (r := r * n ; n := n - 1)|})) true

(** 4. DOT export tests *)

let test_dot () =
  section "DOT export";

  let g = cfg_of "def main with input x output y as if x < 5 then y := 1 else y := 0" in
  let dot = Minimp_cfg_dot.cfg_to_dot ~name:"test" g in

  check_bool "dot: starts with digraph"
    (String.length dot > 8 && String.sub dot 0 8 = "digraph ") true;

  check_bool "dot: contains 'true'"
    (let re = Str.regexp_string "true" in
     try ignore (Str.search_forward re dot 0); true
     with Not_found -> false) true;

  check_bool "dot: contains 'false'"
    (let re = Str.regexp_string "false" in
     try ignore (Str.search_forward re dot 0); true
     with Not_found -> false) true;

  check_bool "dot: contains entry node id"
    (let re = Str.regexp_string (string_of_int g.entry) in
     try ignore (Str.search_forward re dot 0); true
     with Not_found -> false) true;

  check_bool "dot: closes with }"
    (let n = String.length dot in
     n > 0 && dot.[n-2] = '}') true

(* CFG png *)
let test_cfg_png () =
  section "CFG PNG Export";

  let src_while_1_to_n = {|def main with input n output s as
    s := 0 ;
    i := 1 ;
    while i < n do
      s := s + i ;
    i := i + 1|} in
  let g1 = cfg_of src_while_1_to_n in
  Minimp_cfg_dot.export_cfg ~name:"while_1_to_n"
    ~dot_file:"cfg_while_1_to_n.dot"
    ~png_file:"cfg_while_1_to_n.png"
    g1;

  check_bool "cfg_png while_1_to_n dot exists"
    (Sys.file_exists "cfg_while_1_to_n.dot") true;
  check_bool "cfg_png while_1_to_n png exists"
    (Sys.file_exists "cfg_while_1_to_n.png") true;

  let src_while_seq = {|def main with input x output y as while x < 1 do x := x + 1 ; y := x|} in
  let g2 = cfg_of src_while_seq in
  Minimp_cfg_dot.export_cfg ~name:"while_seq"
    ~dot_file:"cfg_while_seq.dot"
    ~png_file:"cfg_while_seq.png"
    g2;

  check_bool "cfg_png while_seq dot exists"
    (Sys.file_exists "cfg_while_seq.dot") true;
  check_bool "cfg_png while_seq png exists"
    (Sys.file_exists "cfg_while_seq.png") true;

  (* ---------------------------------------------------------------- *)
  (* Data-flow annotated CFG exports — plain + defined + live + reach  *)
  (* generated for every test program.                                 *)
  (* ---------------------------------------------------------------- *)

  let export_all_df label src =
    let prog = parse src in
    let g = cfg_of_program prog in
    let base = "cfg_df_" ^ label in

    Minimp_cfg_dot.export_cfg
      ~name:(label ^ "_plain")
      ~dot_file:(base ^ "_plain.dot")
      ~png_file:(base ^ "_plain.png")
      g;

    let (def_cfg, warns) = analyse_defined prog g in
    List.iter (fun (nid, v) ->
      Printf.printf "  %s\n" (pp_undef_warning nid v)) warns;
    Minimp_cfg_dot.export_df_ss_cfg
      ~name:(label ^ "_defined")
      ~dot_file:(base ^ "_defined.dot")
      ~png_file:(base ^ "_defined.png")
      def_cfg;

    let live_cfg = analyse_live prog g in
    Minimp_cfg_dot.export_df_ss_cfg
      ~name:(label ^ "_live")
      ~dot_file:(base ^ "_live.dot")
      ~png_file:(base ^ "_live.png")
      live_cfg;

    let (reach_cfg, all_defs, _) = analyse_reaching prog g in
    Minimp_cfg_dot.export_df_is_cfg
      ~name:(label ^ "_reach")
      ~dot_file:(base ^ "_reach.dot")
      ~png_file:(base ^ "_reach.png")
      all_defs reach_cfg;

    List.iter (fun suffix ->
      check_bool (label ^ ": " ^ suffix ^ " dot exists")
        (Sys.file_exists (base ^ "_" ^ suffix ^ ".dot")) true
    ) ["plain"; "defined"; "live"; "reach"]
  in

  export_all_df "straight"
  "def main with input inp output out as x := inp ; out := x + 1";

  export_all_df "if"
    "def main with input inp output out as if inp < 0 then out := 0 else out := inp";

  export_all_df "while"
    {|def main with input inp output out as
      out := 0 ;
      while inp < 10 do (
        inp := inp + 1 ;
        out := out + 1
      )|};

  export_all_df "undef"
    "def main with input inp output out as if inp < 0 then x := 1 else skip ; out := x";

  export_all_df "redef"
    "def main with input inp output out as x := 1 ; x := 2 ; out := x";

  export_all_df "dead"
    {|def main with input inp output out as
      a := 3 ;
      b := a + inp ;
      out := inp|};
()

(** 5. Data-Flow Analysis tests *)
let test_dataflow () =
  section "Data-Flow Analysis";

  (* ================= Defined Variables ================= *)
  section "  Defined Variables";

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

  (* ================= Live Variables ================= *)
  section "  Live Variables";

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

  (* ================= Reaching Definitions ================= *)
  section "  Reaching Definitions";

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
  Printf.printf "MiniImp Test Suite\n";
  test_eval ();
  test_parser ();
  test_cfg ();
  test_dot ();
  test_cfg_png ();
  test_dataflow ();
  summary ()