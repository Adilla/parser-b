open Blib

let continue_on_error = ref false
let file_mode = ref false

let print_error err =
  Error.print_error err;
  if not !continue_on_error then exit(1)

let print_error_no_loc msg =
  Printf.fprintf stderr "%s\n" msg;
  if not !continue_on_error then exit(1)

let parse_component (graph:Graph.t) (filename:string) : unit =
  try
    let input = open_in filename in
    match Parser.parse_component_from_channel ~filename input with
    | Ok c -> Graph.add_component graph c
    | Error err -> print_error err
  with
  | Sys_error msg -> print_error_no_loc msg

let set_alstm_opt () =
  Typechecker.allow_becomes_such_that_in_implementation := true;
  Typechecker.allow_out_parameters_in_precondition := true;
  Visibility.extended_sees := true

let args = [
  ("-c"    , Arg.Set continue_on_error,   "Continue on error" );
  ("-f"    , Arg.Set file_mode,   "Produce a db file from components" );
  ("-v", Arg.Unit (fun () -> Log.set_verbose true) , "Verbose mode" );
  ("-keep-macro-loc", Arg.Set MacroLexer.keep_macro_loc, "Keep macro locations");
  ("-x", Arg.Unit set_alstm_opt, "(no documentation)" );
]

let rec get_project = function
  | [] -> assert false (*FIXME*)
  | hd::tl ->
    if Xml.tag hd = "project" then hd
    else get_project tl 

let add_path x =
  match File.add_path x with
  | Ok _ -> ()
  | Error err -> print_error_no_loc err

let get_components (lst:string list) (xml:Xml.xml) : string list =
  let tag = Xml.tag xml in
  if tag = "component_file" then
    let name = Xml.attrib xml "name" in
    let path = Xml.attrib xml "path" in
    let suffix = Xml.attrib xml "suffix" in
    (path ^ "/" ^ name ^ "." ^ suffix)::lst
  else if tag = "definition_dir" then
    (add_path (Xml.attrib xml "path"); lst )
  else
    assert false (*FIXME*)

let get_files_from_db (filename:string) : string list =
  let xml = Xml.parse_file filename in
  assert (Xml.tag xml = "db_xml");
  let prj = get_project (Xml.children xml) in
  Xml.fold get_components [] prj
          
let files = ref []

let run_on_file (filename:string) : unit =
  files := filename::!files

let make_graph (files:string list) : Graph.t =
  let graph = Graph.create () in
  let () = List.iter (parse_component graph) files in
  graph

let _ =
  Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files");
  let files =
    if !file_mode then
      !files
    else
      match !files with
      | [] -> assert false (*FIXME*)
      | [db_file] -> get_files_from_db db_file
      | _::_ -> assert false (*FIXME*)
  in
  let graph = make_graph files in
  let stats = Graph.get_statistics graph in
  Printf.fprintf stdout "Number of components: %i\n" stats.components;
  Printf.fprintf stdout "  * Machines: %i\n" stats.machines;
  Printf.fprintf stdout "  * Refinements: %i\n" stats.refinements;
  Printf.fprintf stdout "  * Implementations: %i\n" stats.implementations;
  Printf.fprintf stdout "Number of toplevel machines: %i\n" stats.toplevel_machines;
  Printf.fprintf stdout "Number of imported or included machines: %i\n" stats.imported_included_machines

