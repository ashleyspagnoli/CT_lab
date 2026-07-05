(** Common helpers for MiniImp test suites. *)

open Minimp_ast
open Minimp_cfg
open Minimp_eval

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
  if raised then begin
    incr passed;
    Printf.printf "  [PASS] %s\n" name
  end else begin
    incr failed;
    Printf.printf "  [FAIL] %s (expected exception)\n" name
  end

let section s =
  Printf.printf "\n=== %s ===\n" s

let summary () =
  Printf.printf "\n--- Results: %d/%d passed" !passed !total;
  if !failed > 0 then Printf.printf ", %d FAILED" !failed;
  Printf.printf " ---\n";
  if !failed > 0 then exit 1

let parse src =
  let lexbuf = Lexing.from_string src in
  Minimp_parser.program Minimp_lexer.token lexbuf

let run src input =
  let prog = parse src in
  eval_program prog input

let cfg_of src =
  cfg_of_program (parse src)

let node_count g =
  Hashtbl.length g.nodes

let entry_code g =
  (Hashtbl.find g.nodes g.entry).code

let all_edges_valid g =
  Hashtbl.fold (fun _ n ok ->
    ok && match n.next with
    | End -> true
    | Next id -> Hashtbl.mem g.nodes id
    | Branch (t, f) -> Hashtbl.mem g.nodes t && Hashtbl.mem g.nodes f
  ) g.nodes true
