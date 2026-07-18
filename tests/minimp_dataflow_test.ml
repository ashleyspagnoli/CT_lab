(** MiniImp - Data-Flow Analysis tests *)

open Minimp_ast
open Minimp_cfg
open Minimp_dataflow
open Minimp_cfg_dot
open Minimp_test_common

let check_set label actual expected =
  check label (SS.elements actual) (List.sort_uniq String.compare expected)
    (fun xs -> "{" ^ String.concat ", " xs ^ "}")

let check_int_set label actual expected =
  check label (IS.elements actual) (List.sort_uniq Int.compare expected)
    (fun xs ->
      "{" ^ String.concat ", " (List.map string_of_int xs) ^ "}")

let export_and_check_ss label cfg =
  let base = "cfg_df_" ^ label in
  let dot_file = base ^ ".dot" in
  let png_file = base ^ ".png" in
  remove_if_exists dot_file;
  remove_if_exists png_file;
  export_df_ss_cfg ~name:label ~dot_file ~png_file cfg;
  check_bool (label ^ ": dot created") (file_nonempty dot_file) true;
  check_bool (label ^ ": png created") (file_nonempty png_file) true

let export_and_check_reaching label cfg all_defs =
  let base = "cfg_df_" ^ label in
  let dot_file = base ^ ".dot" in
  let png_file = base ^ ".png" in
  remove_if_exists dot_file;
  remove_if_exists png_file;
  export_df_is_cfg ~name:label ~dot_file ~png_file all_defs cfg;
  check_bool (label ^ ": dot created") (file_nonempty dot_file) true;
  check_bool (label ^ ": png created") (file_nonempty png_file) true

let node_ids_using variable cfg =
  Hashtbl.fold
    (fun id node ids ->
      if List.exists (fun stmt -> SS.mem variable (used_stmt stmt)) node.code
      then id :: ids
      else ids)
    cfg.nodes []
  |> List.sort_uniq Int.compare

let warnings_for variable warnings =
  List.filter_map
    (fun (node_id, warned_variable) ->
      if warned_variable = variable then Some node_id else None)
    warnings
  |> List.sort_uniq Int.compare

let test_defined_partial_branch () =
  let prog = parse
    "def main with input inp output out as if inp < 0 then x := 1 else skip ; out := x"
  in
  let cfg, warnings = analyse_defined prog (cfg_of_program prog) in
  check "warnings: exact variables"
    (List.map snd warnings |> List.sort_uniq String.compare)
    ["x"]
    (fun xs -> String.concat ", " xs);
  check "warnings: x reported in the block that uses it"
    (warnings_for "x" warnings) (node_ids_using "x" cfg)
    (fun xs -> String.concat ", " (List.map string_of_int xs));
  check_bool "warnings: input is defined"
    (List.exists (fun (_, variable) -> variable = "inp") warnings) false;
  export_and_check_ss "partial_branch" cfg

let test_defined_loop () =
  let prog = parse
    {|def main with input inp output out as
      x := 2 ;
      while x < 0 do ( t := x + 3 ; x := y ) ;
      out := 1 - y|}
  in
  let cfg, warnings = analyse_defined prog (cfg_of_program prog) in
  check "warnings: exact variables"
    (List.map snd warnings |> List.sort_uniq String.compare)
    ["y"]
    (fun xs -> String.concat ", " xs);
  check "warnings: every use of y is reported"
    (warnings_for "y" warnings) (node_ids_using "y" cfg)
    (fun xs -> String.concat ", " (List.map string_of_int xs));
  check_bool "warnings: x is definitely defined"
    (List.exists (fun (_, variable) -> variable = "x") warnings) false;
  export_and_check_ss "def_vars_loop" cfg

let test_live_unused_assignment () =
  let prog = parse
    {|def main with input inp output out as
      a := 3 ;
      b := a + inp ;
      out := inp|}
  in
  let cfg = analyse_live prog (cfg_of_program prog) in
  let entry_ann = (Hashtbl.find cfg.nodes cfg.entry).ann in
  let exit_ann = (Hashtbl.find cfg.nodes cfg.exit).ann in
  check_set "live: exact entry in" entry_ann.df_in ["inp"];
  check_set "live: exact exit out" exit_ann.df_out ["out"];
  check_bool "live: unused b is not live at entry" (SS.mem "b" entry_ann.df_in) false;
  export_and_check_ss "live_unused_assignment" cfg

let test_live_if () =
  let prog = parse
    {|def main with input inp output out as
      a := 3 ;
      b := 2 ;
      x := 0 ;
      if 0 < inp then c := a + inp else a := 8 * b;
      c := 2 + a ;
      out := 2 * c + b|}
  in
  let cfg = analyse_live prog (cfg_of_program prog) in
  let entry_ann = (Hashtbl.find cfg.nodes cfg.entry).ann in
  let exit_ann = (Hashtbl.find cfg.nodes cfg.exit).ann in
  check_set "live: exact entry in" entry_ann.df_in ["inp"];
  check_set "live: exact exit in" exit_ann.df_in ["a"; "b"];
  check_set "live: exact exit out" exit_ann.df_out ["out"];
  check_bool "live: unused x is not live at entry" (SS.mem "x" entry_ann.df_in) false;
  export_and_check_ss "live_vars_if" cfg

let test_reaching_last_definition () =
  let prog = parse
    "def main with input inp output out as x := 1 ; x := 2 ; out := x"
  in
  let cfg, all_defs, _ = analyse_reaching prog (cfg_of_program prog) in
  let exit_out = (Hashtbl.find cfg.nodes cfg.exit).ann.df_out in
  let x_defs = List.filter (fun definition -> definition.def_var = "x") all_defs in
  let reaching_x =
    List.filter (fun definition -> IS.mem definition.def_id exit_out) x_defs
  in
  check_int "reaching: two definitions of x indexed" (List.length x_defs) 2;
  check_int "reaching: only the last x definition reaches exit" (List.length reaching_x) 1;
  check_bool "reaching: the second x assignment reaches exit"
    (match reaching_x with [definition] -> definition.def_idx = 1 | _ -> false) true;
  export_and_check_reaching "reaching_last_definition" cfg all_defs

let test_reaching_loop () =
  let prog = parse
    {|def main with input inp output out as
      x := inp ;
      while x < 0 do ( y := x + 3 ; x := y ) ;
      out := 1 - y|}
  in
  let cfg, all_defs, _ = analyse_reaching prog (cfg_of_program prog) in
  let def3 = List.find (fun d -> d.def_id = 3) all_defs in
  let body_ann = (Hashtbl.find cfg.nodes def3.def_node).ann in
  check_bool "reaching: definition 1 enters loop body" (IS.mem 1 body_ann.df_in) true;
  check_bool "reaching: definition 1 is killed" (IS.mem 1 body_ann.df_out) false;
  check_bool "reaching: definition 3 reaches body output" (IS.mem 3 body_ann.df_out) true;
  export_and_check_reaching "reaching_def_loop" cfg all_defs

let test_dataflow_cases () =
  section "Data-Flow Analysis and annotated CFG export";
  let cases = [
    "Defined Variables (partial branching)", test_defined_partial_branch;
    "Defined Variables (loops)", test_defined_loop;
    "Live variables (unused assignments)", test_live_unused_assignment;
    "Live variables (if statements)", test_live_if;
    "Reaching definitions (last definition)", test_reaching_last_definition;
    "Reaching definitions (loops)", test_reaching_loop;
  ] in
  List.iter
    (fun (label, run) ->
      Printf.printf "\n%s\n" label;
      run ())
    cases

let () =
  Printf.printf "Data-Flow Analysis Tests\n";
  test_dataflow_cases ();
  summary ()
