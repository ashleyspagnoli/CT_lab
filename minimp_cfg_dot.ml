(** Export CFG to Graphviz DOT format *)

open Minimp_cfg

(* Escape special characters for DOT labels *)
let dot_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '<' -> Buffer.add_string buf "\\<"
    | '>' -> Buffer.add_string buf "\\>"
    | '{' -> Buffer.add_string buf "\\{"
    | '}' -> Buffer.add_string buf "\\}"
    | '|' -> Buffer.add_string buf "\\|"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

(** Render one CFG to a DOT string *)
let cfg_to_dot ?(name = "cfg") ?(pp_ann : ('a -> string) = fun _ -> "") (g : 'a cfg) : string =
  let buf = Buffer.create 512 in
  let p fmt = Printf.bprintf buf fmt in

  p "digraph %s {\n" name;
  p "rankdir=TB;\n";
  p "node [shape=box, fontname=\"Roboto\", fontsize=11];\n";
  p "edge [fontname=\"Courier\", fontsize=10];\n\n";

  (* Invisible entry/exit anchors *)
  p "entry [label=\"\" shape=point style=invis width=0 height=0];\n";
  p "exit  [label=\"\" shape=point style=invis width=0 height=0];\n\n";

  let ids = sorted_ids g in

  (* Nodes *)
  List.iter (fun id ->
    let n = Hashtbl.find g.nodes id in
    let code_label = dot_escape (pp_block n.code) in
    let ann_label  = dot_escape (pp_ann n.ann) in
    let full_label =
      if ann_label = "" then code_label
      else code_label ^ "\\n" ^ ann_label
    in
    let style =
      if id = g.entry && id = g.exit then
        "style=filled fillcolor=\"#b2d8b2\"" (* entry=exit: green *)
      else if id = g.entry then
        "style=filled fillcolor=\"#b2d8b2\"" (* entry: green *)
      else if id = g.exit then
        "style=filled fillcolor=\"#a8c3f4\"" (* exit: blue *)
      else ""
    in
    p "%d [label=\"%s\"%s];\n" id full_label style
  ) ids;
  p "\n";

  (* Entry / exit pseudo-edges *)
  p "entry -> %d;\n" g.entry;
  p "%d -> exit;\n\n" g.exit;

  (* Real edges *)
  List.iter (fun id ->
    let n = Hashtbl.find g.nodes id in
    (match n.next with
     | End -> ()
     | Next tgt -> p "%d -> %d;\n" id tgt
     | Branch (t_id, f_id) ->
         p "%d -> %d [label=\"true\" color=darkgreen fontcolor=darkgreen];\n" id t_id;
         p "%d -> %d [label=\"false\" color=red fontcolor=red];\n" id f_id)
  ) ids;
  p "}\n";
  Buffer.contents buf

(** Write a DOT file and render a PNG *)
let export_cfg ?(name = "cfg") ?(dot_file = "cfg.dot") ?(png_file = "cfg.png") ?(render = true) 
              ?(pp_ann : ('a -> string) = fun _ -> "") (g : 'a cfg) : unit =
  let dot = cfg_to_dot ~name ~pp_ann g in

  (* Write .dot *)
  let oc = open_out dot_file in
  output_string oc dot;
  close_out oc;
  Printf.printf "DOT written to %s\n%!" dot_file;

  (* Render PNG *)
  if render then begin
    let cmd = Printf.sprintf "dot -Tpng %s -o %s" dot_file png_file in
    match Sys.command cmd with
    | 0 -> Printf.printf "PNG rendered to %s\n%!" png_file
    | _ -> Printf.printf "Graphviz not found or failed (skipping PNG).\n%!"
  end

(** Wrappers for data-flow annotated CFGs *)

open Minimp_dataflow

(* Export a defined-variables or live-variables annotated CFG *)
let export_df_ss_cfg ?(name = "cfg") ?(dot_file = "cfg.dot") ?(png_file = "cfg.png") ?(render = true) (g : SS.t df_ann cfg) : unit =
  let buf = Buffer.create 512 in
  let p fmt = Printf.bprintf buf fmt in

  p "digraph %s {\n" name;
  p "rankdir=TB;\n";
  p "node [shape=box, fontname=\"Roboto\", fontsize=11];\n";
  p "edge [fontname=\"Courier\", fontsize=10];\n\n";
  p "entry [label=\"\" shape=point style=invis width=0 height=0];\n";
  p "exit  [label=\"\" shape=point style=invis width=0 height=0];\n\n";

  let ids = sorted_ids g in
  List.iter (fun id ->
    let n = Hashtbl.find g.nodes id in
    let code_label = dot_escape (pp_block n.code) in
    let full_label =
      Printf.sprintf "in=%s\\n%s\\nout=%s"
        (dot_escape (pp_ss n.ann.df_in))
        code_label
        (dot_escape (pp_ss n.ann.df_out))
    in
    let style =
      if id = g.entry && id = g.exit then
        "style=filled fillcolor=\"#b2d8b2\""
      else if id = g.entry then
        "style=filled fillcolor=\"#b2d8b2\""
      else if id = g.exit then
        "style=filled fillcolor=\"#a8c3f4\""
      else ""
    in
    p "%d [label=\"%s\" %s];\n" id full_label style
  ) ids;
  p "\n";

  p "entry -> %d;\n" g.entry;
  p "%d -> exit;\n\n" g.exit;

  List.iter (fun id ->
    let n = Hashtbl.find g.nodes id in
    match n.next with
    | End -> ()
    | Next tgt -> p "%d -> %d;\n" id tgt
    | Branch (t_id, f_id) ->
        p "%d -> %d [label=\"true\" color=darkgreen fontcolor=darkgreen];\n" id t_id;
        p "%d -> %d [label=\"false\" color=red fontcolor=red];\n" id f_id
  ) ids;
  p "}\n";

  let dot = Buffer.contents buf in
  let oc = open_out dot_file in
  output_string oc dot;
  close_out oc;
  Printf.printf "DOT written to %s\n%!" dot_file;

  if render then begin
    let cmd = Printf.sprintf "dot -Tpng %s -o %s" dot_file png_file in
    match Sys.command cmd with
    | 0 -> Printf.printf "PNG rendered to %s\n%!" png_file
    | _ -> Printf.printf "Graphviz not found or failed (skipping PNG).\n%!"
  end

(* Export a reaching-definitions annotated CFG *)
let export_df_is_cfg ?(name = "cfg") ?(dot_file = "cfg.dot") ?(png_file = "cfg.png") ?(render = true) 
                    (all_defs : def_site list) (g : IS.t df_ann cfg) : unit =
  let buf = Buffer.create 512 in
  let p fmt = Printf.bprintf buf fmt in

  p "digraph %s {\n" name;
  p "rankdir=TB;\n";
  p "node [shape=box, fontname=\"Roboto\", fontsize=11];\n";
  p "edge [fontname=\"Courier\", fontsize=10];\n\n";
  p "entry [label=\"\" shape=point style=invis width=0 height=0];\n";
  p "exit [label=\"\" shape=point style=invis width=0 height=0];\n\n";

  let ids = sorted_ids g in
  List.iter (fun id ->
    let n = Hashtbl.find g.nodes id in
    let code_label = dot_escape (pp_block_reach id n.code all_defs) in
    let full_label =
      Printf.sprintf "in=%s\\n%s\\nout=%s"
        (dot_escape (pp_reach_ids n.ann.df_in))
        code_label
        (dot_escape (pp_reach_ids n.ann.df_out))
    in
    let style =
      if id = g.entry && id = g.exit then " style=filled fillcolor=\"#b2d8b2\""
      else if id = g.entry then " style=filled fillcolor=\"#b2d8b2\""
      else if id = g.exit then " style=filled fillcolor=\"#a8c3f4\""
      else ""
    in
    p "%d [label=\"%s\"%s];\n" id full_label style
  ) ids;
  p "\n";
  p "entry -> %d;\n" g.entry;
  p "%d -> exit;\n\n" g.exit;
  List.iter (fun id ->
    let n = Hashtbl.find g.nodes id in
    (match n.next with
     | End -> ()
     | Next tgt -> p "%d -> %d;\n" id tgt
     | Branch (t_id, f_id) ->
         p "%d -> %d [label=\"true\" color=darkgreen fontcolor=darkgreen];\n" id t_id;
         p "%d -> %d [label=\"false\" color=red fontcolor=red];\n" id f_id)
  ) ids;
  p "}\n";

  let dot = Buffer.contents buf in
  let oc = open_out dot_file in
  output_string oc dot;
  close_out oc;
  Printf.printf "DOT written to %s\n%!" dot_file;
  if render then begin
    let cmd = Printf.sprintf "dot -Tpng %s -o %s" dot_file png_file in
    match Sys.command cmd with
    | 0 -> Printf.printf "PNG rendered to %s\n%!" png_file
    | rc -> Printf.printf "Graphviz not found or failed."
  end
