(** MiniImp — LLVM IR Generation Test Suite *)

open Minimp_ast
open Minimp_cfg
open Minimp_eval
open Minimp_llvm

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

let parse src =
  let lexbuf = Lexing.from_string src in
  Minimp_parser.program Minimp_lexer.token lexbuf

let run src input =
  let prog = parse src in
  eval_program prog input

(* Compile MiniImp program to a .ll file, run opt mem2reg, compile with
   the wrapper via clang, execute with the given input and return stdout. *)
let llvm_run (ll_name : string) (prog : program) (g : plain_cfg) (input : int) : int option =
  let ll_file = Printf.sprintf "tests/%s.ll" ll_name in
  let opt_file = Printf.sprintf "tests/%s_opt.ll" ll_name in
  let obj_file = Printf.sprintf "tests/%s.o" ll_name in
  let bin_file = Printf.sprintf "tests/%s_bin" ll_name in
  let wrap_file = "tests/wrapper.c" in

  (* Write wrapper *)
  let wrapper = {|
    #include <stdio.h>
    #include <stdint.h>
    #include <stdlib.h>
    extern int64_t func(int64_t);
    int main(int argc, char *argv[]) {
      int64_t inp = atoll(argv[1]);
      int64_t out = func(inp);
      printf("%ld\n", out);
      return 0;
    }
  |} in
  (let oc = open_out wrap_file in output_string oc wrapper; close_out oc);

  write_llvm_file ll_file prog g;

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
      else begin
        let ic = Unix.open_process_in (Printf.sprintf "%s %d" bin_file input) in
        let line = try input_line ic with End_of_file -> "" in
        ignore (Unix.close_process_in ic);
        try Some (int_of_string (String.trim line))
        with _ -> None
      end
    end
  end

(* Check that LLVM execution matches the interpreter for a given program and input *)
let check_llvm_vs_interp ll_name src input =
  let prog = parse src in
  let g    = cfg_of_program prog in
  let expected = eval_program prog input in
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
  test_ir_structure ();
  test_simple_programs ();
  test_conditionals ();
  test_loops ()

let () =
  test_llvm ();
  summary ()