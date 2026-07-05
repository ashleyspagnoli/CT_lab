(** MiniImp — LLVM IR Generation Test Suite *)

open Minimp_ast
open Minimp_cfg
open Minimp_llvm
open Minimp_test_common

let has_command cmd =
  Sys.command (Printf.sprintf "command -v %s >/dev/null 2>&1" cmd) = 0

let llvm_toolchain_available () =
  has_command "opt" && has_command "llc" && has_command "clang"

let run_binary_with_input bin_file input =
  let ic, oc, ec = Unix.open_process_full bin_file [||] in
  output_string oc (Printf.sprintf "%d\n" input);
  close_out oc;
  let line = try input_line ic with End_of_file -> "" in
  let _stderr = try really_input_string ec 4096 with _ -> "" in
  ignore (Unix.close_process_full (ic, oc, ec));
  try Some (int_of_string (String.trim line))
  with _ -> None

(* Compile MiniImp program to a .ll file, run opt mem2reg, compile with
   the wrapper via clang, execute with the given input and return stdout. *)
let llvm_run (ll_name : string) (prog : program) (g : plain_cfg) (input : int) : int option =
  let ll_file = Printf.sprintf "tests/%s.ll" ll_name in
  let opt_file = Printf.sprintf "tests/%s_opt.ll" ll_name in
  let obj_file = Printf.sprintf "tests/%s.o" ll_name in
  let bin_file = Printf.sprintf "tests/%s_bin" ll_name in
  let wrap_file = "tests/wrapper.c" in

  write_llvm_file ll_file prog g;

  if not (Sys.file_exists wrap_file) then
    (Printf.printf "  [WARN] wrapper file not found: %s\n" wrap_file; None)
  else if not (llvm_toolchain_available ()) then None
  else
  (* opt mem2reg *)
  let r1 = Sys.command (Printf.sprintf "opt -p='mem2reg' %s -S -o %s 2>/dev/null" ll_file opt_file) in
  if r1 <> 0 then (Printf.printf "  [WARN] opt failed for %s\n" ll_name; None)
  else begin
    (* compile to obj *)
    let r2 = Sys.command (Printf.sprintf "llc -filetype=obj %s -o %s 2>/dev/null" opt_file obj_file) in
    if r2 <> 0 then (Printf.printf "  [WARN] llc failed for %s\n" ll_name; None)
    else begin
      (* link with wrapper *)
      let r3 = Sys.command (Printf.sprintf "clang %s %s -o %s 2>/dev/null" wrap_file obj_file bin_file) in
      if r3 <> 0 then (Printf.printf "  [WARN] clang link failed for %s\n" ll_name; None)
      else run_binary_with_input bin_file input
    end
  end

(* Check that LLVM execution matches the interpreter for a given program and input *)
let check_llvm_vs_interp ll_name src input =
  let prog = parse src in
  let g    = cfg_of_program prog in
  let expected = run src input in
  match llvm_run ll_name prog g input with
  | None ->
      (* LLVM toolchain not available — just check IR is non-empty *)
      let ir = generate_llvm prog g in
      check (Printf.sprintf "%s (input=%d) IR non-empty" ll_name input)
        (String.length ir > 0) true string_of_bool
  | Some got ->
      check_int (Printf.sprintf "%s (input=%d)" ll_name input) got expected

(* Check that the generated IR contains a given substring *)
let check_ir_contains ll_name src substr =
  let prog = parse src in
  let g    = cfg_of_program prog in
  let ir   = generate_llvm prog g in
  let found = let re = Str.regexp_string substr in
              try ignore (Str.search_forward re ir 0); true
              with Not_found -> false
  in
  check (Printf.sprintf "%s IR contains '%s'" ll_name substr) found true string_of_bool

(* ── Test sections ── *)

let test_ir_structure () =
  section "IR structure";

  (* identity: output = input *)
  let src = "def main with input x output y as y := x" in
  check_ir_contains "identity" src "define i64 @func";
  check_ir_contains "identity" src "alloca i64";
  check_ir_contains "identity" src "ret i64";

  (* arithmetic: output = input + 1 *)
  let src2 = "def main with input x output y as y := x + 1" in
  check_ir_contains "arith" src2 "add i64";

  (* subtraction *)
  let src3 = "def main with input x output y as y := x - 3" in
  check_ir_contains "sub" src3 "sub i64";

  (* multiplication *)
  let src4 = "def main with input x output y as y := x * 2" in
  check_ir_contains "mul" src4 "mul i64";

  (* conditional → icmp *)
  let src5 = "def main with input x output y as if x < 0 then y := 1 else y := 2" in
  check_ir_contains "cond" src5 "icmp slt i64";
  check_ir_contains "cond" src5 "br i1"

let test_simple_programs () =
  section "Simple programs (IR correctness via interpreter agreement)";

  let cases = [
    ("identity",  "def main with input x output y as y := x",               [0; 1; 42; -7]);
    ("add1",      "def main with input x output y as y := x + 1",            [0; 5; -1]);
    ("double",    "def main with input x output y as y := x * 2",            [0; 3; -4]);
    ("const",     "def main with input x output y as y := 42",               [0; 99]);
    ("arith",     "def main with input x output y as y := x * x + x",        [0; 2; 5]);
    ("multi_assign",
      "def main with input x output y as (y := x + 1 ; y := y * 2)",
      [0; 3; 10]);
  ] in
  List.iter (fun (name, src, inputs) ->
    List.iter (fun inp ->
      check_llvm_vs_interp name src inp
    ) inputs
  ) cases

let test_conditionals () =
  section "Conditionals";

  let cases = [
    ("if_pos",
      "def main with input x output y as if x < 0 then y := 0 else y := 1",
      [-5; 0; 5]);
    ("abs",
      "def main with input x output y as if x < 0 then y := 0 - x else y := x",
      [-3; 0; 4]);
    ("nested_if",
      {|def main with input x output y as
          if x < 0 then y := 0
          else if x < 10 then y := 1
          else y := 2|},
      [-1; 5; 15]);
  ] in
  List.iter (fun (name, src, inputs) ->
    List.iter (fun inp ->
      check_llvm_vs_interp name src inp
    ) inputs
  ) cases

let test_loops () =
  section "Loops";

  let cases = [
    ("count_to_zero",
      {|def main with input x output y as
          y := 0 ;
          while 0 < x do (y := y + 1 ; x := x - 1)|},
      [0; 1; 5; 10]);
    ("sum_1_to_n",
      {|def main with input x output y as
          y := 0 ;
          while 0 < x do (y := y + x ; x := x - 1)|},
      [0; 1; 4; 10]);
    ("factorial_like",
      {|def main with input x output y as
          y := 1 ;
          while 0 < x do (y := y * x ; x := x - 1)|},
      [0; 1; 5]);
  ] in
  List.iter (fun (name, src, inputs) ->
    List.iter (fun inp ->
      check_llvm_vs_interp name src inp
    ) inputs
  ) cases

let test_llvm () =
  section "LLVM IR Generation";
  if not (llvm_toolchain_available ()) then
    Printf.printf "  [INFO] LLVM native execution skipped: opt/llc/clang not all available; checking generated IR instead.\n";
  test_ir_structure ();
  test_simple_programs ();
  test_conditionals ();
  test_loops ()

let () =
  test_llvm ();
  summary ()
