(** Run every MiniImp and MiniFun test suite, then print aggregate stats. *)

type suite = {
  name : string;
  cmd : string;
}

type result = {
  passed : int;
  failed : int;
  status_ok : bool;
}

let suites = [
  { name = "MiniImp Parser / Lexer"; cmd = "./tests/minimp_parser_test" };
  { name = "MiniImp Evaluator"; cmd = "./tests/minimp_eval_test" };
  { name = "MiniImp CFG"; cmd = "./tests/minimp_cfg_test" };
  { name = "MiniImp Data-Flow"; cmd = "./tests/minimp_dataflow_test" };
  { name = "MiniImp Optimiser"; cmd = "./tests/minimp_opt_test" };
  { name = "MiniImp LLVM"; cmd = "./tests/minimp_llvm_test" };
  { name = "MiniFun"; cmd = "./tests/minifun_test" };
]

let result_line_re = Str.regexp "--- Results:"

let find_result_line lines =
  List.find_opt (fun line -> Str.string_match result_line_re line 0) (List.rev lines)

let parse_minimp_summary line =
  try
    Scanf.sscanf line "--- Results: %d/%d passed%[^-]---"
      (fun passed total _ -> Some (passed, total - passed))
  with _ -> None

let parse_minifun_summary line =
  try
    Scanf.sscanf line "--- Results: %d passed, %d failed ---"
      (fun passed failed -> Some (passed, failed))
  with _ -> None

let parse_summary lines =
  match find_result_line lines with
  | None -> None
  | Some line ->
      match parse_minimp_summary line with
      | Some _ as parsed -> parsed
      | None -> parse_minifun_summary line

let status_ok = function
  | Unix.WEXITED 0 -> true
  | _ -> false

let run_suite suite =
  Printf.printf "\n##### %s #####\n%!" suite.name;
  let ic = Unix.open_process_in suite.cmd in
  let rec read acc =
    match input_line ic with
    | line ->
        print_endline line;
        read (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  let lines = read [] in
  let status = Unix.close_process_in ic in
  let passed, failed =
    match parse_summary lines with
    | Some counts -> counts
    | None ->
        Printf.printf "  [WARN] Could not parse summary for %s\n" suite.name;
        (0, 1)
  in
  let failed = if status_ok status then failed else failed + 1 in
  { passed; failed; status_ok = status_ok status }

let () =
  let results = List.map run_suite suites in
  let passed = List.fold_left (fun acc r -> acc + r.passed) 0 results in
  let failed = List.fold_left (fun acc r -> acc + r.failed) 0 results in
  let failed_suites =
    List.fold_left (fun acc r -> if r.status_ok then acc else acc + 1) 0 results
  in
  Printf.printf "\n===== Aggregate Results =====\n";
  Printf.printf "Suites: %d run, %d failed\n" (List.length suites) failed_suites;
  Printf.printf "Tests:  %d/%d passed" passed (passed + failed);
  if failed > 0 then Printf.printf ", %d FAILED" failed;
  Printf.printf "\n%!";
  if failed > 0 then exit 1
