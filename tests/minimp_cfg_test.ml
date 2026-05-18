(** minimp_cfg_test.ml — smoke-tests for Fragment 5, with DOT export *)

open Minimp_ast
open Minimp_cfg
open Minimp_cfg_dot

(* ------------------------------------------------------------------ *)
let run_test name dot_name prog =
  Printf.printf "=== %s ===\n" name;
  let g = cfg_of_program prog in
  print_string (pp_cfg g);
  print_newline ();
  export_cfg ~name:dot_name
             ~dot_file:(dot_name ^ ".dot")
             ~png_file:(dot_name ^ ".png")
             g

(* ------------------------------------------------------------------ *)
(** Test 1: example from slide 7
    def main with input y output x as
      x := 2;
      if y < 0 then (y := x + 3; x := y)
               else  x := 1 - y
*)
let prog1 = {
  input_var  = "y";
  output_var = "x";
  body =
    Seq (
      Assign ("x", Num 2),
      If (BoolLt (Var "y", Num 0),
        Seq (Assign ("y", BinOp (Var "x", Add, Num 3)),
             Assign ("x", Var "y")),
        Assign ("x", BinOp (Num 1, Sub, Var "y"))))
}

(* ------------------------------------------------------------------ *)
(** Test 2: binomial example from slide 3
    if k > n-k then k := n-k else skip;
    c := 1; i := 0;
    while i < k do (
      c := c * (n-i);
      c := c / (i+1);   (* note: MiniImp has no Div, using Sub as stand-in *)
      i := i + 1
    );
    out := c
    We approximate c/… as c - 0 since MiniImp has no division.           *)
let prog2 = {
  input_var  = "n";
  output_var = "out";
  body =
    Seq (
      Seq (
        Seq (
          If (BoolLt (BinOp (Var "n", Sub, Var "k"), Var "k"),   (* k > n-k  ≡  n-k < k *)
              Assign ("k", BinOp (Var "n", Sub, Var "k")),
              Skip),
          Seq (Assign ("c", Num 1), Assign ("i", Num 0))),
        While (BoolLt (Var "i", Var "k"),
          Seq (
            Seq (
              Assign ("c", BinOp (Var "c", Mul, BinOp (Var "n", Sub, Var "i"))),
              Assign ("c", BinOp (Var "c", Sub, Num 0))),   (* stand-in for c/(i+1) *)
            Assign ("i", BinOp (Var "i", Add, Num 1))))),
      Assign ("out", Var "c"))
}

(* ------------------------------------------------------------------ *)
(** Test 3: simple while loop — verifies back-edge *)
let prog3 = {
  input_var  = "n";
  output_var = "n";
  body = While (BoolLt (Num 0, Var "n"),
           Assign ("n", BinOp (Var "n", Sub, Num 1)))
}

(* ------------------------------------------------------------------ *)
let () =
  run_test "slide-7 if-example" "cfg_if" prog1;
  run_test "slide-3 binomial" "cfg_binomial" prog2;
  run_test "simple while" "cfg_while"    prog3