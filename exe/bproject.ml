open Blib
let continue_on_error = ref false
let generate_db = ref false

type machine_interface = Global.t_interface
type t_item = Done of machine_interface option | InProgress

let open_in (fn:string) : in_channel option =
  try Some (open_in fn) with Sys_error _ -> None

type interface_table = {
  itf:(string,t_item) Hashtbl.t;
  paths: (string,string) Hashtbl.t;
  graph:  Graph.t
}

let safe_find ht s =
  try Some (Hashtbl.find ht.itf s)
  with Not_found -> None

let print_error err =
  Error.print_error err;
  if not !continue_on_error then exit(1)

let print_error_no_loc msg =
  Printf.fprintf stderr "%s\n" msg;
  if not !continue_on_error then exit(1)

let rec type_component (ht:interface_table) (filename:string) : machine_interface option Error.t_result =
  match safe_find ht filename with
  | Some (Done itf) -> Ok itf
  | Some InProgress ->
    Error { Error.err_loc=Utils.dloc;
            Error.err_txt="Error: dependency cycle detected." }
  | None ->
    begin match open_in filename with
      | None -> Error { Error.err_loc=Utils.dloc; 
                        Error.err_txt="Cannot open file '"^filename^"'." }
      | Some input ->
        let () = Log.write "Parsing file '%s'...\n%!" filename in
        begin match Parser.parse_component_from_channel ~filename input with
          | Ok c ->
            let () = close_in input in
            let () = Log.write "Typing file '%s'...\n%!" filename in
            let () = Hashtbl.add ht.itf filename InProgress in
            begin match Typechecker.type_component (f ht) c with
              | Ok (cmp,itf) ->
                begin
                  Hashtbl.add ht.itf filename (Done itf);
                  Graph.add_component ht.graph cmp;
                  Ok itf
                end
              | Error _ as err -> err
            end
          | Error _ as err -> err
        end
    end

and f (ht:interface_table) (mch_loc:Utils.loc) (mch_name:string) : machine_interface option =
  match Hashtbl.find_opt ht.paths mch_name with
  | None ->
    let err = { Error.err_loc=mch_loc; err_txt="Cannot find machine '"^mch_name^"'." } in
    ( Error.print_error err; None )
  | Some fn ->
   begin match type_component ht fn with
     | Ok (Some ok) -> Some ok
     | Ok None ->
       let err = { Error.err_loc=mch_loc; err_txt="The component '"^mch_name^"' is an implementation." } in (*FIXME*)
       ( Error.print_error err; None )
     | Error err -> ( print_error err; None )
   end


let typecheck_file ht (component_name:string) : unit =
  let () = Log.write "Processing component '%s'...\n%!" component_name in
  match type_component ht component_name with
  | Error err -> print_error err
  | Ok _ -> ()

let set_alstm_opt () =
  Typechecker.allow_becomes_such_that_in_implementation := true;
  Typechecker.allow_out_parameters_in_precondition := true;
  Visibility.extended_sees := true

let long = ref false

let args = [
  ("-c"    , Arg.Set continue_on_error,   "Continue on error" );
  ("-m"    , Arg.Set generate_db,   "Produce a db file from components" );
  ("-l"    , Arg.Set long,   "Continue on error" );
  ("-v", Arg.Unit (fun () -> Log.set_verbose true) , "Verbose mode" );
  ("-keep-macro-loc", Arg.Set MacroLexer.keep_macro_loc, "Keep macro locations");
  ("-x", Arg.Unit set_alstm_opt, "(no documentation)" );
]

let rec get_project = function
  | [] -> assert false (*FIXME*)
  | hd::tl ->
    if Xml.tag hd = "project" then hd
    else get_project tl 

type component_file = {
  name:string;
  path:string;
  suffix:string;
}

let add_path x =
  match File.add_path x with
  | Ok _ -> ()
  | Error err -> print_error_no_loc err

let get_components lst xml =
  let tag = Xml.tag xml in
  if tag = "component_file" then
    { name=Xml.attrib xml "name";
      path=Xml.attrib xml "path";
      suffix=Xml.attrib xml "suffix"; }::lst
  else if tag = "definition_dir" then
    (add_path (Xml.attrib xml "path"); lst )
  else
    assert false (*FIXME*)

let process_db (filename:string) : unit =
  let xml = Xml.parse_file filename in
  assert (Xml.tag xml = "db_xml");
  let prj = get_project (Xml.children xml) in
  let cmps = Xml.fold get_components [] prj in
  let ht:interface_table = { itf=Hashtbl.create 47; paths=Hashtbl.create 47; graph=Graph.create () } in
  let () = List.iter (fun cmp -> Hashtbl.add ht.paths cmp.name (cmp.path ^ "/" ^ cmp.name ^ "." ^ cmp.suffix)) cmps in
  let () =
    try List.iter (fun cmp -> typecheck_file ht (cmp.path ^ "/" ^ cmp.name ^ "." ^ cmp.suffix)) cmps
    with Error.Error err -> print_error err
  in
  let stats = Graph.get_statistics ht.graph in
  (*FIXME project name*)
  Printf.fprintf stdout "Number of components: %i\n" stats.components;
  Printf.fprintf stdout "  * Machines: %i\n" stats.machines;
  Printf.fprintf stdout "  * Refinements: %i\n" stats.refinements;
  Printf.fprintf stdout "  * Implementations: %i\n" stats.implementations;
  Printf.fprintf stdout "Number of toplevel machines: %i\n" stats.toplevel_machines;
  Printf.fprintf stdout "Number of imported or included machines: %i\n" stats.imported_included_machines;
  if !long then
    begin
      Graph.iter (
        fun mch_name infos ->
          match infos with
          | Graph.Machine { imported; includes; sees; seen; included; _ } ->
            begin
              Printf.fprintf stdout "Machine %s\n" mch_name;
              (match imported with
               | None -> ()
               | Some imported ->
                 Printf.fprintf stdout "  Imported by %s\n" imported);
              List.iter
                (fun r -> Printf.fprintf stdout "  Refined by %s\n" r)
                (Graph.get_refinements ht.graph mch_name);
              List.iter
                (fun r -> Printf.fprintf stdout "  Includes %s\n" r)
                includes;
              List.iter
                (fun r -> Printf.fprintf stdout "  Sees %s\n" r)
                sees;
              List.iter
                (fun r -> Printf.fprintf stdout "  Seen by %s\n" r)
                seen;
              List.iter
                (fun r -> Printf.fprintf stdout "  Included by %s\n" r)
                included
            end
          | Graph.Refinement infos ->
            begin
              Printf.fprintf stdout "Refinement %s\n" mch_name;
              Printf.fprintf stdout "  Refines %s\n" infos.refines;
              List.iter
                (fun r -> Printf.fprintf stdout "  Refined by %s\n" r)
                (Graph.get_refinements ht.graph mch_name);
              List.iter
                (fun r -> Printf.fprintf stdout "  Includes %s\n" r)
                infos.includes;
              List.iter
                (fun r -> Printf.fprintf stdout "  Sees %s\n" r)
                infos.sees
            end
          | Graph.Implementation infos ->
            begin
              Printf.fprintf stdout "Implementation %s\n" mch_name;
              Printf.fprintf stdout "  Refines %s\n" infos.refines;
              List.iter
                (fun r -> Printf.fprintf stdout "  Imports %s\n" r)
                infos.imports;
              List.iter
                (fun r -> Printf.fprintf stdout "  Sees %s\n" r)
                infos.sees
            end
      ) ht.graph
    end

let files = ref []

let run_on_file (filename:string) : unit =
  files := filename::!files

let print_db (files:string list) : unit =
  let cmps = List.rev_map (fun filename ->
      if Sys.file_exists filename then
        let filename =
          if Filename.is_relative filename then
            Filename.current_dir_name ^ "/" ^ filename
          else filename
        in
        let path = Filename.dirname filename in
        let basename = Filename.basename filename in
        let name = Filename.remove_extension basename in
        let ext = Filename.extension basename in
        let suffix =
          if ext = ".mch" then "mch"
          else if ext = ".ref" then "ref"
          else if ext = ".imp" then "imp"
          else assert false (*FIXME*)
        in
        Xml.Element("component_file",[("path",path);("name",name);("suffix",suffix)],[])
      else
        assert false (*FIXME*)
    ) files
  in
  let doc = Xml.Element ("db_xml",[],[Xml.Element ("project",[],cmps)]) in
  Printf.fprintf (open_out "out.db") "%s" (Xml.to_string_fmt doc)

let _ =
  Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files");
  if !generate_db then print_db !files
  else List.iter process_db !files
