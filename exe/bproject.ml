open Blib
let continue_on_error = ref false

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

let args = [
  ("-c"    , Arg.Set continue_on_error,   "Continue on error" );
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

let run_on_file (filename:string) : unit =
  let xml = Xml.parse_file filename in
  assert (Xml.tag xml = "db_xml");
  let prj = get_project (Xml.children xml) in
  let cmps = Xml.fold get_components [] prj in
  let ht:interface_table = { itf=Hashtbl.create 47; paths=Hashtbl.create 47; graph=Graph.create () } in
  let () = List.iter (fun cmp -> Hashtbl.add ht.paths cmp.name (cmp.path ^ "/" ^ cmp.name ^ "." ^ cmp.suffix)) cmps in
  let () = List.iter (fun cmp -> typecheck_file ht (cmp.path ^ "/" ^ cmp.name ^ "." ^ cmp.suffix)) cmps in
  let stats = Graph.get_statistics ht.graph in
  (*FIXME project name*)
  Printf.fprintf stdout "Number of components: %i\n" stats.components;
  Printf.fprintf stdout "  * Machines: %i\n" stats.machines;
  Printf.fprintf stdout "  * Refinements: %i\n" stats.refinements;
  Printf.fprintf stdout "  * Implementations: %i\n" stats.implementations;
  Printf.fprintf stdout "Number of toplevel machines: %i\n" stats.toplevel_machines;
  Printf.fprintf stdout "  * Base Machines: %i\n" stats.toplevel_base_machines;
  Printf.fprintf stdout "  * Implemented Machines: %i\n" (stats.toplevel_machines - stats.toplevel_base_machines);
  Printf.fprintf stdout "Number of imported machines: %i\n" stats.imported_machines;
  Printf.fprintf stdout "  * Base Machines: %i\n" stats.imported_base_machines;
  Printf.fprintf stdout "  * Implemented Machines: %i\n" (stats.imported_machines-stats.imported_base_machines);
  Printf.fprintf stdout "Number of included machines: %i\n" stats.included_machines;
  Graph.iter (
    fun mch_name infos ->
      match infos with
      | Graph.RootMachine infos ->
        begin
          Printf.fprintf stdout "Toplevel machine %s\n" mch_name;
          List.iter
            (fun r -> Printf.fprintf stdout "  Refined by %s\n" r)
            (Graph.get_refinements ht.graph mch_name);
          List.iter
            (fun r -> Printf.fprintf stdout "  Includes %s\n" r)
            infos.includes;
          List.iter
            (fun r -> Printf.fprintf stdout "  Sees %s\n" r)
            infos.sees;
          List.iter
            (fun r -> Printf.fprintf stdout "  Seen by %s\n" r)
            infos.seen
        end
      | Graph.ImportedMachine infos ->
        begin
          Printf.fprintf stdout "Imported machine %s\n" mch_name;
          Printf.fprintf stdout "  Imported by %s\n" infos.imported;
          List.iter
            (fun r -> Printf.fprintf stdout "  Refined by %s\n" r)
            (Graph.get_refinements ht.graph mch_name);
          List.iter
            (fun r -> Printf.fprintf stdout "  Includes %s\n" r)
            infos.includes;
          List.iter
            (fun r -> Printf.fprintf stdout "  Sees %s\n" r)
            infos.sees;
          List.iter
            (fun r -> Printf.fprintf stdout "  Seen by %s\n" r)
            infos.seen
        end
      | Graph.IncludedMachine infos ->
        begin
          Printf.fprintf stdout "Included machine %s\n" mch_name;
          List.iter
            (fun r -> Printf.fprintf stdout "  Included by %s\n" r)
            infos.included;
          List.iter
            (fun r -> Printf.fprintf stdout "  Includes %s\n" r)
            infos.includes;
          List.iter
            (fun r -> Printf.fprintf stdout "  Sees %s\n" r)
            infos.sees
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

let _ = Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files")
