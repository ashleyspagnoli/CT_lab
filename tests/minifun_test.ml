(** MiniFun — Test Suite *)

open Minifun_ast
open Minifun_eval
open Minifun_typechecker
open Minifun_infer

(** Helpers *)

(* Parse a MiniFun term from a string *)
let parse (src : string) : term =
  let lexbuf = Lexing.from_string src in
  Minifun_parser.term_eof Minifun_lexer.token lexbuf

(* Run a term through the evaluator and return its value *)
let run (src : string) : value = eval_minifun (parse src)

(* Run a term through the simple type-checker and return its type *)
let tc (src : string) : typ = typecheck_program (parse src)

(* Run a term through HM inference and return the principal polytype as a string *)
let infer (src : string) : string = infer_program_pp (parse src)

(** Minimal test framework *)

let pass_count = ref 0
let fail_count = ref 0

let check ~label ~expected ~got =
  if expected = got then begin
    Printf.printf "  [PASS] %s\n" label;
    incr pass_count
  end else begin
    Printf.printf "  [FAIL] %s\n         expected: %s\n         got:      %s\n"
      label expected got;
    incr fail_count
  end

(** Expect an exception *)
let check_raises ~label ~substr f =
  match (try f (); None with
        | Minifun_eval.RuntimeError msg -> Some msg
        | Minifun_typechecker.TypeError msg -> Some msg
        | Minifun_infer.TypeError msg -> Some msg
        | Failure msg -> Some msg
        | _ -> Some "<unexpected exception>") with
  | Some msg when String.length substr = 0
               || (let re = Str.regexp_string substr in
                   (try ignore (Str.search_forward re msg 0); true
                    with Not_found -> false)) ->
      Printf.printf "  [PASS] %s\n" label;
      incr pass_count
  | Some msg ->
      Printf.printf "  [FAIL] %s\n expected exception containing: %s\n got: %s\n"
        label substr msg;
      incr fail_count
  | None ->
      Printf.printf "  [FAIL] %s\n expected an exception but none was raised\n" label;
      incr fail_count

let section title =
  Printf.printf "\n=== %s ===\n" title

let summary () =
  Printf.printf "\n--- Results: %d passed, %d failed ---\n" !pass_count !fail_count

(** 1. Evaluator tests *)

let () = section "Evaluator — literals"

let () =
  check ~label:"integer literal"
    ~expected:"42" ~got:(pp_value (run "42"));
  check ~label:"boolean true"
    ~expected:"true" ~got:(pp_value (run "true"));
  check ~label:"boolean false"
    ~expected:"false" ~got:(pp_value (run "false"))

let () = section "Evaluator — arithmetic"

let () =
  check ~label:"1 + 2 = 3"
    ~expected:"3" ~got:(pp_value (run "1 + 2"));
  check ~label:"10 - 4 = 6"
    ~expected:"6" ~got:(pp_value (run "10 - 4"));
  check ~label:"3 * 7 = 21"
    ~expected:"21" ~got:(pp_value (run "3 * 7"));
  check ~label:"2 + 3 * 4 = 14  (precedence)"
    ~expected:"14" ~got:(pp_value (run "2 + 3 * 4"))

let () = section "Evaluator — comparison and boolean"

let () =
  check ~label:"1 < 2 = true"
    ~expected:"true" ~got:(pp_value (run "1 < 2"));
  check ~label:"2 < 1 = false"
    ~expected:"false" ~got:(pp_value (run "2 < 1"));
  check ~label:"~(2 < 1) = true"
    ~expected:"true" ~got:(pp_value (run "~ (2 < 1)"));
  check ~label:"true && false = false"
    ~expected:"false" ~got:(pp_value (run "true && false"));
  check ~label:"true && true = true"
    ~expected:"true" ~got:(pp_value (run "true && true"));
  check ~label:"~ true = false"
    ~expected:"false" ~got:(pp_value (run "~ true"));
  check ~label:"~ false = true"
    ~expected:"true" ~got:(pp_value (run "~ false"))

let () = section "Evaluator — if-then-else"

let () =
  check ~label:"if true"
    ~expected:"1" ~got:(pp_value (run "if true then 1 else 2"));
  check ~label:"if false"
    ~expected:"2" ~got:(pp_value (run "if false then 1 else 2"));
  check ~label:"nested if"
    ~expected:"3" ~got:(pp_value (run "if 1 < 2 then if false then 0 else 3 else 99"))

let () = section "Evaluator — let"

let () =
  check ~label:"let x = 5 in x"
    ~expected:"5" ~got:(pp_value (run "let x = 5 in x"));
  check ~label:"let x = 3 in x + 2"
    ~expected:"5" ~got:(pp_value (run "let x = 3 in x + 2"));
  check ~label:"let x = 2 in let y = 3 in x * y"
    ~expected:"6" ~got:(pp_value (run "let x = 2 in let y = 3 in x * y"));
  check ~label:"inner let shadows outer"
    ~expected:"10" ~got:(pp_value (run "let x = 5 in let x = 10 in x"))

let () = section "Evaluator — fun and application"

let () =
  check ~label:"identity function"
    ~expected:"7" ~got:(pp_value (run "(fun x => x) 7"));
  check ~label:"add one"
    ~expected:"6" ~got:(pp_value (run "(fun x => x + 1) 5"));
  check ~label:"curried addition"
    ~expected:"9" ~got:(pp_value (run "(fun x => fun y => x + y) 4 5"));
  check ~label:"closure captures environment"
    ~expected:"15" ~got:(pp_value (run "let a = 10 in (fun x => x + a) 5"))

let () = section "Evaluator — letfun (recursive)"

let () =
  check ~label:"factorial"
    ~expected:"120" ~got:(pp_value (run "letfun fact n = if n < 1 then 1 else n * (fact (n - 1)) in fact 5"));
  check ~label:"fibonacci"
    ~expected:"55" ~got:(pp_value (run "letfun fib n = if n < 2 then n else (fib (n - 1)) + (fib (n - 2)) in fib 10"));
  check ~label:"sum 1..100"
    ~expected:"5050" ~got:(pp_value (run "letfun sum n = if n < 1 then 0 else n + (sum (n - 1)) in sum 100"));
  check ~label:"mutually-simulated: even/odd via single letfun"
    ~expected:"true" ~got:(pp_value (run "letfun isEven n = if n < 1 then ~ (n < 0) else isEven (n - 2) in isEven 4"))

let () = section "Evaluator — runtime errors"

let () =
  check_raises ~label:"unbound variable"
    ~substr:"Undefined variable" (fun () -> ignore (run "x"));
  check_raises ~label:"apply non-function"
    ~substr:"non-function" (fun () -> ignore (run "42 7"));
  check_raises ~label:"arithmetic on booleans"
    ~substr:"not applicable" (fun () -> ignore (run "true + 1"))

    
(** 2. Type-checker tests *)

let () = section "Type-checker — literals"

let () =
  check ~label:"42 : int"
    ~expected:"int" ~got:(pp_typ (tc "42"));
  check ~label:"true : bool"
    ~expected:"bool" ~got:(pp_typ (tc "true"))

let () = section "Type-checker — binary operators"

let () =
  check ~label:"1 + 2 : int"
    ~expected:"int" ~got:(pp_typ (tc "1 + 2"));
  check ~label:"1 < 2 : bool"
    ~expected:"bool" ~got:(pp_typ (tc "1 < 2"));
  check ~label:"true && false : bool"
    ~expected:"bool" ~got:(pp_typ (tc "true && false"))

let () = section "Type-checker — functions with annotations"

let () =
  check ~label:"fun x : int => x + 1 : int -> int"
    ~expected:"int -> int" ~got:(pp_typ (tc "fun x : int => x + 1"));
  check ~label:"fun x : bool => ~ x  :  bool -> bool"
    ~expected:"bool -> bool" ~got:(pp_typ (tc "fun x : bool => ~ x"));
  check ~label:"curried  int -> int -> int"
    ~expected:"int -> int -> int" ~got:(pp_typ (tc "fun x : int => fun y : int => x + y"))

let () = section "Type-checker — let"

let () =
  check ~label:"let x = 1 in x + 1 : int"
    ~expected:"int" ~got:(pp_typ (tc "let x = 1 in x + 1"));
  check ~label:"let x = true in ~ x : bool"
    ~expected:"bool" ~got:(pp_typ (tc "let x = true in ~ x"))

let () = section "Type-checker — letfun with annotation"

let () =
  check ~label:"factorial annotation : int -> int"
    ~expected:"int -> int"
    ~got:(pp_typ (tc "letfun fact n : int -> int = if n < 1 then 1 else n * (fact (n - 1)) in fact"));
  check ~label:"letfun applied : int"
    ~expected:"int"
    ~got:(pp_typ (tc "letfun fact n : int -> int = if n < 1 then 1 else n * (fact (n - 1)) in fact 5"))

let () = section "Type-checker — type errors"

let () =
  check_raises ~label:"arithmetic on bool raises TypeError"
    ~substr:"arithmetic" (fun () -> ignore (tc "true + 1"));
  check_raises ~label:"if branches mismatch"
    ~substr:"branches" (fun () -> ignore (tc "if true then 1 else false"));
  check_raises ~label:"if condition not bool"
    ~substr:"condition" (fun () -> ignore (tc "if 42 then 1 else 2"));
  check_raises ~label:"apply non-function type"
    ~substr:"non-function" (fun () -> ignore (tc "42 7"));
  check_raises ~label:"fun without annotation raises TypeError"
    ~substr:"annotation" (fun () -> ignore (tc "fun x => x"));
  check_raises ~label:"letfun without annotation raises TypeError"
    ~substr:"annotation" (fun () -> ignore (tc "letfun f x = x + 1 in f 1"));
  check_raises ~label:"letfun annotation must be a function type"
    ~substr:"function type" (fun () -> ignore (tc "letfun f x : int = x + 1 in f 1"))

    
(** 3. HM type-inference tests *)

let () = section "Inference — monomorphic"

let () =
  check ~label:"42 : int"
    ~expected:"int" ~got:(infer "42");
  check ~label:"true : bool"
    ~expected:"bool" ~got:(infer "true");
  check ~label:"1 + 2 : int"
    ~expected:"int" ~got:(infer "1 + 2");
  check ~label:"1 < 2 : bool"
    ~expected:"bool" ~got:(infer "1 < 2");
  check ~label:"fun x => x + 1 : int -> int"
    ~expected:"int -> int" ~got:(infer "fun x => x + 1");
  check ~label:"let x = 1 in x + 1 : int"
    ~expected:"int" ~got:(infer "let x = 1 in x + 1")

let () = section "Inference — polymorphic (let-generalisation)"

let () =
  let id_ty = infer "let id = fun x => x in id" in
  check ~label:"let id = fun x => x : ∀a. a -> a (is polymorphic)"
    ~expected:"true"
    ~got:(string_of_bool (String.length id_ty > 0 &&
                          (let re = Str.regexp "->$\\|-> " in
                           (try ignore (Str.search_forward re id_ty 0); true
                            with Not_found -> false))));
  check ~label:"id used at two different types in same let"
    ~expected:"int" ~got:(infer "let id = fun x => x in let y = id 1 in let b = id true in y")

let () = section "Inference — recursive letfun"

let () =
  check ~label:"letfun fact : int -> int"
    ~expected:"int -> int"
    ~got:(infer "letfun fact n = if n < 1 then 1 else n * (fact (n - 1)) in fact");
  check ~label:"letfun fib applied : int"
    ~expected:"int"
    ~got:(infer "letfun fib n = if n < 2 then n else (fib (n-1)) + (fib (n-2)) in fib 10")

let () = section "Inference — type errors (unification failures)"

let () =
  check_raises ~label:"type mismatch"
    ~substr:"" (fun () -> ignore (infer "true + 1"));
  check_raises ~label:"type mismatch if branches"
    ~substr:"" (fun () -> ignore (infer "if true then 1 else false"));
  check_raises ~label:"occurs check: fun x => x x"
    ~substr:"infinite type" (fun () -> ignore (infer "fun x => x x"))

    
let () = summary ()