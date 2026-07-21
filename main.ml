(** Single entry point to test MiniImp and MiniFun *)

let read_file (filename : string) : string =
  let ic = open_in_bin filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let usage (prog : string) : unit =
  Printf.eprintf "Usage:\n";
  Printf.eprintf "  %s <file.imp> <input>                  run a MiniImp program\n" prog;
  Printf.eprintf "  %s <file.imp> --cfg [b]                export the CFG as <b>.dot/.png\n" prog;
  Printf.eprintf "  %s <file.imp> --dataflow <kind> [b]    export a data-flow analysis CFG (kind: live|reach|defined)\n" prog;
  Printf.eprintf "  %s <file.imp> --opt [b]                optimize the program and export the resulting CFG\n" prog;
  Printf.eprintf "  %s <file.imp> <input> --opt [b]        optimize, export the CFG, and run the program\n" prog;
  Printf.eprintf "  %s <file.imp> --llvm [b]               compile to LLVM IR\n" prog;
  Printf.eprintf "  %s <file.imp> <input> --llvm-run [b]   generate, optimise, compile, and run LLVM\n" prog;
  Printf.eprintf "  %s <file.fun> <input>                  infer the type and evaluate a MiniFun program\n" prog;
  Printf.eprintf "  %s <file.fun> --check                  type-check (explicit annotations required)\n" prog;
  Printf.eprintf "  %s <file.fun> <input> --check          type-check the application and evaluate it\n" prog

let run_command (description : string) (command : string) : unit =
  if Sys.command command <> 0 then begin
    Printf.eprintf "LLVM error while %s\n" description;
    exit 1
  end

let compile_and_run_llvm (prog : Minimp_ast.program) (input_val : int) (basename : string) : unit =
  let ll_file = basename ^ ".ll" in
  let opt_file = basename ^ "_opt.ll" in
  let obj_file = basename ^ ".o" in
  let bin_file = basename ^ "_bin" in
  let g = Minimp_cfg.cfg_of_program prog in
  Minimp_llvm.write_llvm_file ll_file prog g;
  run_command "running mem2reg"
    (Printf.sprintf "opt -p='mem2reg' %s -S -o %s" (Filename.quote ll_file) (Filename.quote opt_file));
  run_command "compiling LLVM IR"
    (Printf.sprintf "llc -filetype=obj %s -o %s" (Filename.quote opt_file) (Filename.quote obj_file));
  run_command "linking the wrapper"
    (Printf.sprintf "clang %s %s -o %s" (Filename.quote "tests/wrapper.c") (Filename.quote obj_file) (Filename.quote bin_file));
  let executable = if Filename.dirname bin_file = "." then "./" ^ bin_file else bin_file
  in
  run_command "running the compiled program"
    (Printf.sprintf "printf '%%s\\n' %s | %s" (Filename.quote (string_of_int input_val)) (Filename.quote executable))

let run_imp (file : string) (src : string) (argv : string array) : unit =
  let lexbuf = Lexing.from_string src in
  let prog =
    try Minimp_parser.program Minimp_lexer.token lexbuf
    with
    | Minimp_parser.Error ->
        Printf.eprintf "Syntax error in %s\n" file;
        exit 1
    | Failure msg ->
        Printf.eprintf "Lexical error in %s: %s\n" file msg;
        exit 1
  in
  let export_optimized basename =
    let g = Minimp_cfg.cfg_of_program prog in
    let warnings = Minimp_opt.check_undefined prog g in
    print_string (Minimp_opt.pp_undefined_warnings warnings);
    print_newline ();
    let optimized = Minimp_opt.optimise prog g in
    Minimp_cfg_dot.export_cfg
      ~name:"cfg_optimized" ~dot_file:(basename ^ ".dot") ~png_file:(basename ^ ".png") ~render:true
      optimized;
    optimized
  in
  if Array.length argv >= 3 && argv.(2) = "--cfg" then begin
    let basename = if Array.length argv >= 4 then argv.(3) else "cfg" in
    let g = Minimp_cfg.cfg_of_program prog in
    Minimp_cfg_dot.export_cfg
      ~name:"cfg" ~dot_file:(basename ^ ".dot") ~png_file:(basename ^ ".png") ~render:true
      g
  end
  else if Array.length argv >= 3 && argv.(2) = "--dataflow" then begin
    if Array.length argv < 4 then begin
      Printf.eprintf "Specify an analysis: live | reach | defined\n";
      exit 1
    end;
    let kind = argv.(3) in
    let default_basename = "dataflow_" ^ kind in
    let basename = if Array.length argv >= 5 then argv.(4) else default_basename in
    let g = Minimp_cfg.cfg_of_program prog in
    (match kind with
     | "live" ->
         let live_cfg = Minimp_dataflow.analyse_live prog g in
         Minimp_cfg_dot.export_df_ss_cfg
           ~name:"live" ~dot_file:(basename ^ ".dot") ~png_file:(basename ^ ".png") ~render:true
           live_cfg
     | "defined" ->
         let (def_cfg, warnings) = Minimp_dataflow.analyse_defined prog g in
         Minimp_cfg_dot.export_df_ss_cfg
           ~name:"defined" ~dot_file:(basename ^ ".dot") ~png_file:(basename ^ ".png") ~render:true
           def_cfg;
         List.iter
           (fun (id, v) -> Printf.printf "Warning: variable '%s' may be undefined\n" v)
           warnings
     | "reach" ->
         let (reach_cfg, all_defs, _) = Minimp_dataflow.analyse_reaching prog g in
         Minimp_cfg_dot.export_df_is_cfg
           ~name:"reach" ~dot_file:(basename ^ ".dot") ~png_file:(basename ^ ".png") ~render:true
           all_defs reach_cfg
     | other ->
         Printf.eprintf "Unknown data-flow analysis '%s' (expected: live | reach | defined)\n" other;
         exit 1)
  end
  else if Array.length argv >= 3 && argv.(2) = "--opt" then begin
    let basename = if Array.length argv >= 4 then argv.(3) else "cfg_optimized" in
    ignore (export_optimized basename)
  end
  else if Array.length argv >= 3 && argv.(2) = "--llvm" then begin
    let out = if Array.length argv >= 4 then argv.(3) else "out.ll" in
    let g = Minimp_cfg.cfg_of_program prog in
    Minimp_llvm.write_llvm_file out prog g
  end
  else begin
    if Array.length argv < 3 then begin
      Printf.eprintf "An input value is required.\n";
      usage argv.(0);
      exit 1
    end;
    let input_val =
      try int_of_string argv.(2)
      with Failure _ ->
        Printf.eprintf "'%s' is not a valid input\n" argv.(2);
        exit 1
    in
    let optimized_cfg =
      if Array.length argv >= 4 && argv.(3) = "--opt" then begin
        let basename = if Array.length argv >= 5 then argv.(4) else "cfg_optimized" in
        Some (export_optimized basename)
      end else
        None
    in
    if Array.length argv >= 4 && argv.(3) = "--llvm-run" then begin
      let basename = if Array.length argv >= 5 then argv.(4) else "out" in
      compile_and_run_llvm prog input_val basename
    end else
    try
      let result =
        match optimized_cfg with
        | Some g -> Minimp_eval.eval_cfg g prog input_val
        | None -> Minimp_eval.eval_program prog input_val
      in
      Printf.printf "%d\n" result
    with Minimp_eval.UndefinedVariable msg ->
      Printf.eprintf "Runtime error: %s\n" msg;
      exit 1
  end

let parse_minifun_term (label : string) (src : string) : Minifun_ast.term =
  let lexbuf = Lexing.from_string src in
  try Minifun_parser.term_eof Minifun_lexer.token lexbuf
  with
  | Minifun_parser.Error ->
      Printf.eprintf "Syntax error in %s\n" label;
      exit 1
  | Failure msg ->
      Printf.eprintf "Lexical error in %s: %s\n" label msg;
      exit 1

let run_fun (file : string) (src : string) (argv : string array) : unit =
  let term = parse_minifun_term file src in
  let has_input = Array.length argv >= 3 && argv.(2) <> "--check" in
  let check_requested =
    (Array.length argv >= 3 && argv.(2) = "--check") ||
    (Array.length argv >= 4 && argv.(3) = "--check")
  in
  let term =
    if has_input then
      let arg = parse_minifun_term "command-line argument" argv.(2) in
      Minifun_ast.TApp (term, arg)
    else
      term
  in

  if check_requested then begin
    try
      let ty = Minifun_typechecker.typecheck_program term in
      Printf.printf "Type: %s\n" (Minifun_typechecker.pp_typ ty)
    with Minifun_typechecker.TypeError msg ->
      Printf.eprintf "Type error: %s\n" msg;
      exit 1
  end;
  if not check_requested || has_input then begin

    (try
       let ty_str = Minifun_infer.infer_program_pp term in
       Printf.printf "Inferred type: %s\n" ty_str
     with Minifun_infer.TypeError msg ->
       Printf.eprintf "Inference error: %s\n" msg);

    try
      let v = Minifun_eval.eval_minifun term in
      Printf.printf "Value: %s\n" (Minifun_eval.pp_value v)
    with Minifun_eval.RuntimeError msg ->
      Printf.eprintf "Runtime error: %s\n" msg;
      exit 1
  end
  
let () =
  let argv = Sys.argv in
  if Array.length argv < 2 then begin
    usage argv.(0);
    exit 1
  end;
  let file = argv.(1) in
  if not (Sys.file_exists file) then begin
    Printf.eprintf "File not found: %s\n" file;
    exit 1
  end;
  let src = read_file file in
  if Filename.check_suffix file ".imp" then run_imp file src argv
  else if Filename.check_suffix file ".fun" then run_fun file src argv
  else begin
    Printf.eprintf "Unrecognized extension (use .imp or .fun): %s\n" file;
    exit 1
  end
