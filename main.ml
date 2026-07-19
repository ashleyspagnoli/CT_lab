(** Single entry point to test MiniImp and MiniFun *)

let read_file (filename : string) : string =
  let ic = open_in_bin filename in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let usage (prog : string) : unit =
  Printf.eprintf "Usage:\n";
  Printf.eprintf "  %s <file.imp> <input_int>              run a MiniImp program\n" prog;
  Printf.eprintf "  %s <file.imp> --cfg [basename]         export the CFG as <basename>.dot/.png\n" prog;
  Printf.eprintf "  %s <file.imp> --dataflow <kind> [b]    export a data-flow analysis (kind: live|reach|defined)\n" prog;
  Printf.eprintf "  %s <file.imp> --optimize [basename]    optimize the program and export the resulting CFG\n" prog;
  Printf.eprintf "  %s <file.imp> --llvm [out.ll]          compile to LLVM IR\n" prog;
  Printf.eprintf "  %s <file.fun>                          infer the type and evaluate a MiniFun program\n" prog;
  Printf.eprintf "  %s <file.fun> --check                  type-check (explicit annotations required)\n" prog

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
  if Array.length argv >= 3 && argv.(2) = "--cfg" then begin
    let basename = if Array.length argv >= 4 then argv.(3) else "cfg" in
    let g = Minimp_cfg.cfg_of_program prog in
    Minimp_cfg_dot.export_cfg
      ~name:"cfg"
      ~dot_file:(basename ^ ".dot")
      ~png_file:(basename ^ ".png")
      ~render:true
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
           (fun (id, v) -> Printf.printf "Warning: variable '%s' may be undefined at node %d\n" v id)
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
  else if Array.length argv >= 3 && argv.(2) = "--optimize" then begin
    let basename = if Array.length argv >= 4 then argv.(3) else "cfg_optimized" in
    let g = Minimp_cfg.cfg_of_program prog in
    let warnings = Minimp_opt.check_undefined prog g in
    print_string (Minimp_opt.pp_undefined_warnings warnings);
    print_newline ();
    let optimized = Minimp_opt.optimise prog g in
    Minimp_cfg_dot.export_cfg
      ~name:"cfg_optimized"
      ~dot_file:(basename ^ ".dot")
      ~png_file:(basename ^ ".png")
      ~render:true
      optimized
  end
  else if Array.length argv >= 3 && argv.(2) = "--llvm" then begin
    let out = if Array.length argv >= 4 then argv.(3) else "out.ll" in
    let g = Minimp_cfg.cfg_of_program prog in
    Minimp_llvm.write_llvm_file out prog g;
    Printf.printf "LLVM IR written to %s\n" out
  end
  else begin
    if Array.length argv < 3 then begin
      Printf.eprintf "An integer input value is required.\n";
      usage argv.(0);
      exit 1
    end;
    let input_val =
      try int_of_string argv.(2)
      with Failure _ ->
        Printf.eprintf "'%s' is not a valid integer\n" argv.(2);
        exit 1
    in
    try
      let result = Minimp_eval.eval_program prog input_val in
      Printf.printf "%d\n" result
    with Minimp_eval.UndefinedVariable msg ->
      Printf.eprintf "Runtime error: %s\n" msg;
      exit 1
  end

let run_fun (file : string) (src : string) (argv : string array) : unit =
  let lexbuf = Lexing.from_string src in
  let term =
    try Minifun_parser.term_eof Minifun_lexer.token lexbuf
    with
    | Minifun_parser.Error ->
        Printf.eprintf "Syntax error in %s\n" file;
        exit 1
    | Failure msg ->
        Printf.eprintf "Lexical error in %s: %s\n" file msg;
        exit 1
  in
  if Array.length argv >= 3 && argv.(2) = "--check" then begin
    try
      let ty = Minifun_typechecker.typecheck_program term in
      Printf.printf "Type: %s\n" (Minifun_typechecker.pp_typ ty)
    with Minifun_typechecker.TypeError msg ->
      Printf.eprintf "Type error: %s\n" msg;
      exit 1
  end
  else begin
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