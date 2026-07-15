(** MiniImp - CFG tests *)

open Minimp_ast
open Minimp_cfg
open Minimp_cfg_dot
open Minimp_test_common

let node g id = Hashtbl.find g.nodes id

let contains s sub =
  try
    ignore (Str.search_forward (Str.regexp_string sub) s 0);
    true
  with Not_found -> false

let has_assign x code =
  List.exists (function SAssign (y, _) -> x = y | _ -> false) code

let is_guard code =
  match code with
  | [SGuard _] -> true
  | _ -> false

let file_nonempty file =
  if not (Sys.file_exists file) then false
  else
    let ic = open_in_bin file in
    let size = in_channel_length ic in
    close_in ic;
    size > 0

let remove_if_exists file =
  if Sys.file_exists file then Sys.remove file

let check_graph label g =
  check_bool (label ^ ": all edges valid") (all_edges_valid g) true;
  check_bool (label ^ ": exit terminal") ((node g g.exit).next = End) true

let test_basic_cfg () =
  section "Basic CFGs";

  let g = cfg_of "def main with input x output y as skip" in
  check_int "skip: one node" (node_count g) 1;
  check_bool "skip: entry=exit" (g.entry = g.exit) true;
  check "skip: correct code" (entry_code g) [SSkip] (fun _ -> "<block>");
  check_graph "skip" g;

  let g = cfg_of "def main with input x output y as y := x" in
  check_int "assign: one node" (node_count g) 1;
  check_bool "assign: entry=exit" (g.entry = g.exit) true;
  check "assign: correct code"
    (entry_code g)
    [SAssign ("y", Var "x")]
    (fun _ -> "<block>");
  check_graph "assign" g;

  let g = cfg_of "def main with input x output y as a := x ; b := a + 1 ; y := b" in
  check_int "sequence: one merged node" (node_count g) 1;
  check_int "sequence: three statements" (List.length (entry_code g)) 3;
  check_bool "sequence: contains a" (has_assign "a" (entry_code g)) true;
  check_bool "sequence: contains b" (has_assign "b" (entry_code g)) true;
  check_bool "sequence: contains y" (has_assign "y" (entry_code g)) true;
  check_graph "sequence" g;

  let g = cfg_of "def main with input x output y as skip ; y := x ; skip" in
  check_int "skip sequence: one node" (node_count g) 1;
  check "skip sequence: redundant skips removed"
    (entry_code g)
    [SAssign ("y", Var "x")]
    (fun _ -> "<block>");
  check_graph "skip sequence" g

let test_if_cfgs () =
  section "If CFGs";

  let g = cfg_of "def main with input x output y as if x < 5 then y := 1 else y := 0" in
  check_bool "if: entry is guard" (is_guard (entry_code g)) true;
  check_bool "if: has two distinct branches"
    (match (node g g.entry).next with
     | Branch (t, f) -> t <> f
     | _ -> false)
    true;
  check_bool "if: both branches assign y"
    (match (node g g.entry).next with
     | Branch (t, f) ->
         has_assign "y" (node g t).code &&
         has_assign "y" (node g f).code
     | _ -> false)
    true;
  check_bool "if: both branches reach join"
    (match (node g g.entry).next with
     | Branch (t, f) ->
         (node g t).next = Next g.exit &&
         (node g f).next = Next g.exit
     | _ -> false)
    true;
  check_int "if: join has two predecessors"
    (List.length (predecessors g g.exit))
    2;
  check_graph "if" g;

  let g = cfg_of "def main with input x output y as if x < 0 then skip else y := x" in
  check_bool "if-skip: entry is guard" (is_guard (entry_code g)) true;
  check_bool "if-skip: skip branch reaches join directly"
    (match (node g g.entry).next with
     | Branch (t, f) -> t = g.exit || f = g.exit
     | _ -> false)
    true;
  check_bool "if-skip: other branch assigns y"
    (match (node g g.entry).next with
     | Branch (t, f) ->
         has_assign "y" (node g t).code ||
         has_assign "y" (node g f).code
     | _ -> false)
    true;
  check_graph "if-skip" g

let test_while_cfgs () =
  section "While CFGs";

  let g = cfg_of "def main with input x output y as while x < 10 do x := x + 1" in
  check_bool "while: entry is guard" (is_guard (entry_code g)) true;
  check_bool "while: false branch is exit"
    (match (node g g.entry).next with
     | Branch (_, f) -> f = g.exit
     | _ -> false)
    true;
  check_bool "while: body assigns x"
    (match (node g g.entry).next with
     | Branch (b, _) -> has_assign "x" (node g b).code
     | _ -> false)
    true;
  check_bool "while: body loops back"
    (match (node g g.entry).next with
     | Branch (b, _) -> (node g b).next = Next g.entry
     | _ -> false)
    true;
  check_graph "while" g;

  let g = cfg_of
    {|def main with input x output y as
      while x < 10 do (
        x := x + 1 ;
        y := x
      )|}
  in
  check_bool "while-seq: body contains x and y"
    (match (node g g.entry).next with
     | Branch (b, _) ->
         has_assign "x" (node g b).code &&
         has_assign "y" (node g b).code
     | _ -> false)
    true;
  check_int "while-seq: body contains two statements"
    (match (node g g.entry).next with
     | Branch (b, _) -> List.length (node g b).code
     | _ -> 0)
    2;
  check_bool "while-seq: body loops back"
    (match (node g g.entry).next with
     | Branch (b, _) -> (node g b).next = Next g.entry
     | _ -> false)
    true;
  check_graph "while-seq" g;

  let g = cfg_of
    {|def main with input x output y as
      while x < 10 do 
        x := x + 1 ;
      y := x|}
  in
  check_bool "while-followed: body contains only x"
    (match (node g g.entry).next with
     | Branch (b, _) ->
         has_assign "x" (node g b).code &&
         not (has_assign "y" (node g b).code)
     | _ -> false)
    true;
  check_bool "while-followed: exit assigns y"
    (has_assign "y" (node g g.exit).code)
    true;
  check_graph "while-followed" g

let test_nested_cfg () =
  section "Nested CFG";

  let g = cfg_of
    {|def main with input n output r as
      r := 0 ;
      while r < n do (
        i := 0 ;
        while i < n do i := i + 1 ;
        r := r + 1
      )|}
  in
  let guards = Hashtbl.fold (fun _ n count -> if is_guard n.code then count + 1 else count) g.nodes 0 in
  check_int "nested: two guards" guards 2;
  check_bool "nested: several CFG nodes" (node_count g >= 5) true;
  check_graph "nested" g

let test_dot () =
  section "DOT generation";

  let g = cfg_of "def main with input x output y as if x < 5 then y := 1 else y := 0" in
  let dot = cfg_to_dot ~name:"test_cfg" g in
  check_bool "dot: graph declaration"
    (contains dot "digraph test_cfg")
    true;
  check_bool "dot: entry edge"
    (contains dot (Printf.sprintf "entry -> %d;" g.entry))
    true;
  check_bool "dot: exit edge"
    (contains dot (Printf.sprintf "%d -> exit;" g.exit))
    true;
  check_bool "dot: true and false labels"
    (contains dot "label=\"true\"" &&
     contains dot "label=\"false\"")
    true;
  check_bool "dot: prints guard and assignments"
    (contains dot "x < 5" &&
     contains dot "y := 1" &&
     contains dot "y := 0")
    true;
  check_bool "dot: closes graph"
    (let n = String.length dot in
     n >= 2 && dot.[n - 2] = '}')
    true

let export_and_check name source =
  let dot_file = name ^ ".dot" in
  let png_file = name ^ ".png" in

  remove_if_exists dot_file;
  remove_if_exists png_file;

  export_cfg ~name ~dot_file ~png_file (cfg_of source);

  check_bool (name ^ ": dot created") (file_nonempty dot_file) true;
  check_bool (name ^ ": png created") (file_nonempty png_file) true

let test_exports () =
  section "DOT and PNG export";

  export_and_check
    "cfg_sequence"
    "def main with input x output y as a := x ; y := a + 1";

  export_and_check
    "cfg_if"
    "def main with input x output y as if x < 0 then y := 0 else y := x";

  export_and_check
    "cfg_while_sequence"
    {|def main with input n output s as
      s := 0 ;
      i := 0 ;
      while i < n do (
        s := s + i ;
        i := i + 1
      )|};

  export_and_check
    "cfg_while_followed"
    {|def main with input x output y as
      while x < 1 do 
        x := x + 1 ;
      y := x|}

let () =
  Printf.printf "CFG Tests\n";
  test_basic_cfg ();
  test_if_cfgs ();
  test_while_cfgs ();
  test_nested_cfg ();
  test_dot ();
  test_exports ();
  summary ()