(** minimp_cfg_test.ml — smoke-tests for Fragment 5, with DOT export *)

open Minimp_ast
open Minimp_cfg
open Minimp_cfg_dot
open Minimp_dataflow
open Minimp_test_common

let test_cfg_builder () =
  section "CFG builder";

  let g_skip = cfg_of "def main with input x output y as skip" in
  check_int "skip: 1 node" (node_count g_skip) 1;
  check_bool "skip: entry=exit" (g_skip.entry = g_skip.exit) true;

  let g_assign = cfg_of "def main with input x output y as y := x" in
  check_int "assign: 1 node" (node_count g_assign) 1;
  check "assign: entry code" (entry_code g_assign) [SAssign ("y", Var "x")]
    (fun _ -> "<block>");

  let g_seq = cfg_of "def main with input x output y as y := x ; y := y + 1" in
  check_int "seq: merged into 1 node" (node_count g_seq) 1;

  let g_if = cfg_of
    "def main with input x output y as if x < 5 then y := 1 else y := 0" in
  check_bool "if: entry is guard"
    (match entry_code g_if with [SGuard _] -> true | _ -> false) true;
  check_bool "if: all edges valid" (all_edges_valid g_if) true;

  let g_while = cfg_of
    "def main with input x output y as while x < 10 do x := x + 1" in
  check_bool "while: entry is guard"
    (match entry_code g_while with [SGuard _] -> true | _ -> false) true;
  check_bool "while: all edges valid" (all_edges_valid g_while) true;

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

let test_dot () =
  section "DOT export";

  let g = cfg_of "def main with input x output y as if x < 5 then y := 1 else y := 0" in
  let dot = cfg_to_dot ~name:"test" g in

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

let test_cfg_png () =
  section "CFG PNG Export";

  let src_while_1_to_n = {|def main with input n output s as
    s := 0 ;
    i := 1 ;
    while i < n do
      s := s + i ;
    i := i + 1|} in
  let g1 = cfg_of src_while_1_to_n in
  export_cfg ~name:"while_1_to_n"
    ~dot_file:"cfg_while_1_to_n.dot"
    ~png_file:"cfg_while_1_to_n.png"
    g1;

  check_bool "cfg_png while_1_to_n dot exists"
    (Sys.file_exists "cfg_while_1_to_n.dot") true;
  check_bool "cfg_png while_1_to_n png exists"
    (Sys.file_exists "cfg_while_1_to_n.png") true;

  let src_while_seq = {|def main with input x output y as while x < 1 do x := x + 1 ; y := x|} in
  let g2 = cfg_of src_while_seq in
  export_cfg ~name:"while_seq"
    ~dot_file:"cfg_while_seq.dot"
    ~png_file:"cfg_while_seq.png"
    g2;

  check_bool "cfg_png while_seq dot exists"
    (Sys.file_exists "cfg_while_seq.dot") true;
  check_bool "cfg_png while_seq png exists"
    (Sys.file_exists "cfg_while_seq.png") true;

  let export_all_df label src =
    let prog = parse src in
    let g = cfg_of_program prog in
    let base = "cfg_df_" ^ label in

    export_cfg
      ~name:(label ^ "_plain")
      ~dot_file:(base ^ "_plain.dot")
      ~png_file:(base ^ "_plain.png")
      g;

    let (def_cfg, warns) = analyse_defined prog g in
    List.iter (fun (nid, v) ->
      Printf.printf "  %s\n" (pp_undef_warning nid v)) warns;
    export_df_ss_cfg
      ~name:(label ^ "_defined")
      ~dot_file:(base ^ "_defined.dot")
      ~png_file:(base ^ "_defined.png")
      def_cfg;

    let live_cfg = analyse_live prog g in
    export_df_ss_cfg
      ~name:(label ^ "_live")
      ~dot_file:(base ^ "_live.dot")
      ~png_file:(base ^ "_live.png")
      live_cfg;

    let (reach_cfg, all_defs, _) = analyse_reaching prog g in
    export_df_is_cfg
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
      out := inp|}

(* ------------------------------------------------------------------ *)
let () =
  Printf.printf "CFG Tests\n";
  test_cfg_builder ();
  test_dot ();
  test_cfg_png ();
  summary ()
