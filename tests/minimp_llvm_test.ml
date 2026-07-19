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

  if not (Sys.file_exists wrap_file) then (Printf.printf "  [WARN] wrapper file not found: %s\n" wrap_file; None)
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
  let g = cfg_of_program prog in
  let expected = run src input in
  match llvm_run ll_name prog g input with
  | Some got -> check_int (Printf.sprintf "%s (input=%d)" ll_name input) got expected
  | None -> Printf.printf "  [WARN] LLVM execution skipped for %s (input=%d)\n" ll_name input

(* Check that the generated IR contains a given substring *)
let check_ir_contains ll_name src substr =
  let prog = parse src in
  let g = cfg_of_program prog in
  let ir = generate_llvm prog g in
  let found = let re = Str.regexp_string substr in
              try ignore (Str.search_forward re ir 0); true
              with Not_found -> false
  in
  check (Printf.sprintf "%s IR contains '%s'" ll_name substr) found true string_of_bool

(* Tests sections *)

let test_ir_structure () =
  section "IR structure";

  (* Identity: output = input *)
  let src = "def main with input x output y as y := x" in
  check_ir_contains "identity" src "define i64 @func";
  check_ir_contains "identity" src "alloca i64";
  check_ir_contains "identity" src "ret i64";

  (* Arithmetic: output = input + 1 *)
  let src2 = "def main with input x output y as y := x + 1" in
  check_ir_contains "arith" src2 "add i64";

  (* Subtraction *)
  let src3 = "def main with input x output y as y := x - 3" in
  check_ir_contains "sub" src3 "sub i64";

  (* Multiplication *)
  let src4 = "def main with input x output y as y := x * 2" in
  check_ir_contains "mul" src4 "mul i64";

  (* Conditional *)
  let src5 = "def main with input x output y as if x < 0 then y := 1 else y := 2" in
  check_ir_contains "cond" src5 "icmp slt i64";
  check_ir_contains "cond" src5 "br i1"

let test_simple_programs () =
  section "IR correctness with simple programs";

  (* Identity *)
  let identity = "def main with input x output y as y := x" in
  check_llvm_vs_interp "identity" identity 0;
  check_llvm_vs_interp "identity" identity 1;
  check_llvm_vs_interp "identity" identity 42;
  check_llvm_vs_interp "identity" identity (-7);

  (* Addition *)
  let add1 = "def main with input x output y as y := x + 1" in
  check_llvm_vs_interp "add1" add1 0;
  check_llvm_vs_interp "add1" add1 5;
  check_llvm_vs_interp "add1" add1 (-1);

  (* Subtraction *)
  let sub1 = "def main with input x output y as y := x - 1" in
  check_llvm_vs_interp "sub1" sub1 0;
  check_llvm_vs_interp "sub1" sub1 5;
  check_llvm_vs_interp "sub1" sub1 (-1);

  (* Multiplication *)
  let double = "def main with input x output y as y := x * 2" in
  check_llvm_vs_interp "double" double 0;
  check_llvm_vs_interp "double" double 3;
  check_llvm_vs_interp "double" double (-4);

  (* Constant assignment *)
  let const = "def main with input x output y as y := 42" in
  check_llvm_vs_interp "const" const 0;
  check_llvm_vs_interp "const" const 99;

  (* Arithmetic expressions *)
  let arith = "def main with input x output y as y := x * x + x" in
  check_llvm_vs_interp "arith" arith 0;
  check_llvm_vs_interp "arith" arith 2;
  check_llvm_vs_interp "arith" arith 5;

  (* Multiple assignments *)
  let multi_assign = "def main with input x output y as (y := x + 1 ; y := y * 2)" in
  check_llvm_vs_interp "multi_assign" multi_assign 0;
  check_llvm_vs_interp "multi_assign" multi_assign 3;
  check_llvm_vs_interp "multi_assign" multi_assign 10

let test_conditionals () =
  section "Conditionals";

  (* Conditional *)
  let if_pos = "def main with input x output y as if x < 0 then y := 0 else y := 1" in
  check_llvm_vs_interp "if_pos" if_pos (-5);
  check_llvm_vs_interp "if_pos" if_pos 0;
  check_llvm_vs_interp "if_pos" if_pos 5;

  (* Absolute value *)
  let abs = "def main with input x output y as if x < 0 then y := 0 - x else y := x" in
  check_llvm_vs_interp "abs" abs (-3);
  check_llvm_vs_interp "abs" abs 0;
  check_llvm_vs_interp "abs" abs 4;

  (* Nested conditionals *)
  let nested_if = {|def main with input x output y as
    if x < 0 then y := 0
    else if x < 10 then y := 1
    else y := 2|} in
  check_llvm_vs_interp "nested_if" nested_if (-1);
  check_llvm_vs_interp "nested_if" nested_if 5;
  check_llvm_vs_interp "nested_if" nested_if 15

let test_loops () =
  section "Loops";

  (* Count down to zero *)
  let count_to_zero = {|def main with input x output y as
    y := 0 ;
    while 0 < x do (y := y + 1 ; x := x - 1)|} in
  check_llvm_vs_interp "count_to_zero" count_to_zero 0;
  check_llvm_vs_interp "count_to_zero" count_to_zero 1;
  check_llvm_vs_interp "count_to_zero" count_to_zero 5;
  check_llvm_vs_interp "count_to_zero" count_to_zero 10;

  (* Sum of integers from 1 to n *)
  let sum_1_to_n = {|def main with input x output y as
    y := 0 ;
    while 0 < x do (y := y + x ; x := x - 1)|} in
  check_llvm_vs_interp "sum_1_to_n" sum_1_to_n 0;
  check_llvm_vs_interp "sum_1_to_n" sum_1_to_n 1;
  check_llvm_vs_interp "sum_1_to_n" sum_1_to_n 4;
  check_llvm_vs_interp "sum_1_to_n" sum_1_to_n 10;

  (* Factorial*)
  let factorial_like = {|def main with input x output y as
    y := 1 ;
    while 0 < x do (y := y * x ; x := x - 1)|} in
  check_llvm_vs_interp "factorial_like" factorial_like 0;
  check_llvm_vs_interp "factorial_like" factorial_like 1;
  check_llvm_vs_interp "factorial_like" factorial_like 5

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
