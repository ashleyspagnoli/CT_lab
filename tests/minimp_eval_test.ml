(** MiniImp - Evaluator tests. *)

open Minimp_test_common

let test_eval () =
  section "Evaluator";

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

  check_int "sequence"
    (run "def main with input x output y as y := x ; y := y + 1" 3) 4;

  check_int "if true branch"
    (run "def main with input x output y as if x < 10 then y := 1 else y := 0" 5) 1;

  check_int "if false branch"
    (run "def main with input x output y as if x < 10 then y := 1 else y := 0" 15) 0;

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

  check_int "bool and - both true"
    (run "def main with input x output y as if x < 10 and 1 < x then y := 1 else y := 0" 5) 1;

  check_int "bool and - one false"
    (run "def main with input x output y as if x < 10 and 1 < x then y := 1 else y := 0" 0) 0;

  check_int "bool not"
    (run "def main with input x output y as if not (x < 5) then y := 1 else y := 0" 7) 1;

  check_raises "undefined variable raises"
    (fun () -> run "def main with input x output y as y := z" 0)

let check_cfg_eval name src input =
  let prog = parse src in
  let cfg = Minimp_cfg.cfg_of_program prog in
  check_int name
    (Minimp_eval.eval_cfg cfg prog input)
    (Minimp_eval.eval_program prog input)

let check_optimized_cfg_eval name src input =
  let prog = parse src in
  let cfg = Minimp_opt.optimise prog (Minimp_cfg.cfg_of_program prog) in
  check_int name
    (Minimp_eval.eval_cfg cfg prog input)
    (Minimp_eval.eval_program prog input)

let test_cfg_eval () =
  section "CFG evaluator";
  check_cfg_eval "sequence"
    "def main with input x output y as y := (x + 1) * 2" 4;
  check_cfg_eval "conditional"
    "def main with input x output y as if x < 0 then y := 0 - x else y := x" (-3);
  check_cfg_eval "loop"
    "def main with input x output y as y := 0 ; while 0 < x do (y := y + x ; x := x - 1)" 4;
  check_optimized_cfg_eval "optimized loop"
    "def main with input x output y as y := 0 ; while 0 < x do (y := y + x ; x := x - 1)" 4

let () =
  Printf.printf "MiniImp Evaluator Tests\n";
  test_eval ();
  test_cfg_eval ();
  summary ()
