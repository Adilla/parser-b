open Blib

let continue_on_error = ref false

let print_error err =
  Error.print_error err;
  if not !continue_on_error then exit(1)

let print_error_no_loc msg =
  Printf.fprintf stderr "%s\n" msg;
  if not !continue_on_error then exit(1)

let rec get_project = function
  | [] -> None
  | hd::tl ->
    if Xml.tag hd = "project" then Some hd
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
    lst

let get_files_from_db (filename:string) : string list Error.t_result =
  let xml = Xml.parse_file filename in
  if (not (String.equal (Xml.tag xml)  "db_xml")) then
    Error { err_loc=Utils.dloc;
            err_txt = "Ill-form db file: root element different from 'db_xml'." }
  else
    match get_project (Xml.children xml) with
    | None ->
      Error { err_loc=Utils.dloc;
              err_txt = "Ill-form db file: no 'project' tag." }
    | Some prj -> Ok (Xml.fold get_components [] prj)

let parse_component (ast_table:(string,PSyntax.component) Hashtbl.t) (graph:Graph.t) (filename:string) : unit =
  try
    let input = open_in filename in
    match Parser.parse_component_from_channel ~filename input with
    | Ok c -> 
      begin
        close_in input;
        Graph.add_component graph c;
        Hashtbl.add ast_table c.co_name.lid_str c
      end
    | Error err ->
      begin
        close_in input;
        print_error err
      end
  with
  | Sys_error msg -> print_error_no_loc msg

let create_cargo_file (project_name:string) : unit =
  let out = open_out (project_name ^ "/Cargo.toml") in
  Printf.fprintf out "\
[package]
name = \"%s\"
version = \"0.1.0\"
authors = [\"You <you@example.com>\"]
[dependencies]
lazy_static = \"1.1.0\"
" project_name;
  close_out out

let create_lib_file (project_name:string) (machines:string list) : unit =
  let out = open_out (project_name ^ "/src/lib.rs") in
  Printf.fprintf out "\
#[macro_use]
extern crate lazy_static;
pub mod state;";
  List.iter (Printf.fprintf out "\npub mod %s;") machines;
  close_out out

type t_item = Done of (TSyntax.component*Global.t_interface option) | InProgress

type interface_table = (string,t_item) Hashtbl.t

let rec type_component ast_table (ht:interface_table) (cname:string) :
  (TSyntax.component*Global.t_interface option) Error.t_result =
  match Hashtbl.find_opt ht cname with
  | Some (Done x) -> Ok x
  | Some InProgress ->
    Error { Error.err_loc=Utils.dloc;
            Error.err_txt="Error: dependency cycle detected." }
  | None ->
    begin match Hashtbl.find_opt ast_table cname with
      | None ->
        Error { Error.err_loc=Utils.dloc;
            Error.err_txt="Error: missing machine '"^cname^"'." }
      | Some ast ->
        let () = Hashtbl.add ht cname InProgress in
        begin match Typechecker.type_component (f ast_table ht) ast with
          | Ok (c,itf) ->
            let () = Hashtbl.add ht cname (Done (c,itf)) in
            Ok (c,itf)
          | Error _ as err -> err
        end
    end

and f ast_table (ht:interface_table) (mch_loc:Utils.loc) (mch_name:string) : Global.t_interface option =
  match type_component ast_table ht mch_name with
  | Ok (_,Some ok) -> Some ok
  | Ok (_,None) ->
    let err = { Error.err_loc=mch_loc;
                err_txt="The component '"^mch_name^"' is an implementation." } in
    ( Error.print_error err; None )
  | Error err -> ( print_error err; None )

let translate_component ast_table ht (graph:Graph.t) (machine:string) : (string*Codegen.t_package) option =
  let cmp = match Graph.get_implementation_name graph machine with
    | None -> machine
    | Some implem -> implem
  in
  match type_component ast_table ht cmp with
  | Ok (c,_) ->
    begin match Codegen.to_package c.co_name c with
      | Ok pkg -> Some (machine,pkg)
      | Error err -> (print_error err; None)
    end
  | Error err -> (print_error err; None)

let print_package (project_name:string) (mch,pkg:string*Codegen.t_package) : unit =
  let out = open_out
      (project_name ^ "/src/" ^ mch ^ ".rs")
  in
  match Rustprint.print_package out mch pkg with
  | Ok () -> close_out out
  | Error err -> print_error err

let create_state_file (project_name:string) (pkgs:(string*Codegen.t_package) list) : unit =
  let out = open_out (project_name ^ "/src/state.rs") in
  Rustprint.print_state out pkgs;
  close_out out

let make_project project_name (files:string list) : unit =
  Unix.mkdir project_name 0o770;
  Unix.mkdir (project_name ^ "/src") 0o770;
  create_cargo_file project_name;
  let graph = Graph.create () in
  let ast_table = Hashtbl.create 47 in
  List.iter (parse_component ast_table graph) files;
  let machines = Graph.get_sorted_machines graph in
  create_lib_file project_name machines;
  let ht = Hashtbl.create 47 in
  let pkgs = Utils.filter_map (translate_component ast_table ht graph) machines in
  List.iter (print_package project_name) pkgs;
  create_state_file project_name pkgs

let make_project_from_db (db_file:string) : unit =
  let project_name = Filename.remove_extension (Filename.basename db_file) in
  match get_files_from_db db_file with
  | Ok cmps -> make_project project_name cmps
  | Error err -> print_error err

let add_path x =
  match File.add_path x with
  | Ok _ -> ()
  | Error err -> print_error_no_loc err

let set_alstm_opt () = (*FIXME options*)
  Typechecker.allow_becomes_such_that_in_implementation := true;
  Typechecker.allow_out_parameters_in_precondition := true;
  Visibility.extended_sees := true

let file_mode = ref false

let args = [
  ("-f", Arg.Set file_mode,   "file mode" );
  ("-c", Arg.Set continue_on_error,   "Continue on error" );
  ("-I", Arg.String add_path, "Path for definitions files" );
  ("-v", Arg.Unit (fun () -> Log.set_verbose true) , "Verbose mode" );
  ("-keep-macro-loc", Arg.Set MacroLexer.keep_macro_loc, "Keep macro locations");
  ("-x", Arg.Unit set_alstm_opt, "(no documentation)" );
]

let files = ref []

let run_on_file filename =
  files := filename::!files

let _ =
  Arg.parse args run_on_file (
    "Usage: "^ Sys.argv.(0) ^" [options] project.db\n" ^
    "Usage: "^ Sys.argv.(0) ^" [options] -f component1 ... componentn"
  );
  try
    if !file_mode then
      make_project "anon" !files
    else
      List.iter make_project_from_db !files
  with
  | Error.Error err -> print_error err
