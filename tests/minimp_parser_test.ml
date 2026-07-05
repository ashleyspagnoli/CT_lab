(** MiniImp - Parser and lexer tests. *)

open Minimp_ast
open Minimp_test_common

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
   | Seq (While _, Assign _) ->
       incr total; incr passed; Printf.printf "  [PASS] while-seq structure\n"
   | _ ->
       incr total; incr failed; Printf.printf "  [FAIL] while-seq structure\n");

  check_raises "lexer error raises"
    (fun () -> parse "def main with input x output y as y := @");

  check_raises "parser error raises"
    (fun () -> parse "def main with input x output y as if then skip else skip")

let () =
  Printf.printf "MiniImp Parser / Lexer Tests\n";
  test_parser ();
  summary ()
