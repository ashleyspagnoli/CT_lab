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

(* Render one CFG to a DOT string *)
let cfg_to_dot ?(name="cfg") (g : cfg) : string =
  let buf = Buffer.create 512 in
  let p fmt = Printf.bprintf buf fmt in

  p "digraph %s {\n" name;
  p "rankdir=TB;\n";
  p "node [shape=box, fontname=\"Courier\", fontsize=11];\n";
  p "edge [fontname=\"Courier\", fontsize=10];\n\n";

  (* Nodes *)
  let ids =
    Hashtbl.fold (fun id _ acc -> id :: acc) g.nodes []
    |> List.sort compare
  in
  List.iter (fun id ->
    let n = Hashtbl.find g.nodes id in
    let label = dot_escape (pp_block n.code) in
    let style =
      if id = g.entry && id = g.exit then
        "style=filled fillcolor=\"#a8d8a8\"" (* entry=exit: green *)
      else if id = g.entry then
        "style=filled fillcolor=\"#a8d8a8\"" (* entry: green *)
      else if id = g.exit then
        "style=filled fillcolor=\"#f4a8a8\"" (* exit: red *)
      else ""
    in
    p "%d [label=\"%s\"%s];\n" id label style
  ) ids;
  p "\n";

  (* Edges *)
  List.iter (fun id ->
    let n = Hashtbl.find g.nodes id in
    (match n.next with
     | End -> () (* no outgoing edge *)
     | Next tgt -> p "%d -> %d;\n" id tgt
     | Branch (t_id, f_id) ->
         p "%d -> %d [label=\"true\" color=darkgreen fontcolor=darkgreen];\n" id t_id;
         p "%d -> %d [label=\"false\" color=red fontcolor=red ];\n" id f_id)
  ) ids;
  p "}\n";
  Buffer.contents buf

(** Write a DOT file and invoke Graphviz to render a PNG *)
let export_cfg
    ?(name="cfg")
    ?(dot_file="cfg.dot")
    ?(png_file="cfg.png")
    ?(render=true)
    (g : cfg) : unit =
  let dot = cfg_to_dot ~name g in

  (* Write .dot *)
  let oc = open_out dot_file in
  output_string oc dot;
  close_out oc;
  Printf.printf "DOT written to %s\n%!" dot_file;
  
  (* Render *)
  if render then begin
    let cmd = Printf.sprintf "dot -Tpng %s -o %s" dot_file png_file in
    match Sys.command cmd with
    | 0 -> Printf.printf "PNG rendered to %s\n%!" png_file
    | rc -> Printf.printf "Graphviz not found or failed."
  end