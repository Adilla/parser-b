open Blib
let continue_on_error = ref false
let nb_errors = ref 0

type machine_interface = Global.t_interface
type t_item = Done of machine_interface option | InProgress

let open_in (fn:string) : in_channel option =
  try Some (open_in fn) with Sys_error _ -> None

type interface_table = (string,t_item) Hashtbl.t

let safe_find ht s =
  try Some (Hashtbl.find ht s)
  with Not_found -> None

let rec type_component_from_filename (ht:interface_table) (filename:string) : machine_interface option =
  match safe_find ht filename with
  | Some (Done itf) -> itf
  | Some InProgress ->
    Error.error Utils.dloc "Error: dependency cycle detected."
  | None ->
    begin match open_in filename with
      | None -> Error.error Utils.dloc ("Cannot open file '"^filename^"'.")
      | Some input ->
        Log.write "Parsing file '%s'...\n%!" filename;
        let c = Parser.parse_component_from_channel ~filename input in
        close_in input;
        Log.write "Typing file '%s'...\n%!" filename;
        Hashtbl.add ht filename InProgress;
        let (_,itf) = Typechecker.type_component (f ht) c in
        Hashtbl.add ht filename (Done itf);
        itf
    end

and f (ht:interface_table) (mch_loc:Utils.loc) (mch_name:string) : machine_interface option =
  match File.get_fullname_comp mch_name with
  | None -> Error.error mch_loc ("Cannot find machine '"^mch_name^"'.")
  | Some fn ->
   begin match type_component_from_filename ht fn with
     | Some ok -> Some ok
     | None -> Error.error mch_loc ("The component '"^mch_name^"' is an implementation.")
   end

let ht:interface_table = Hashtbl.create 47

let run_on_file (filename:string) : unit =
  try
    Log.write "Processing file '%s'...\n%!" filename;
    ignore(type_component_from_filename ht filename)
  with
  | Error.Fatal ->
    begin
      incr nb_errors;
      if not !continue_on_error then raise Exit
    end

let add_path x =
  try File.add_path x
  with Error.Fatal -> ()

let args = [
  ("-c"    , Arg.Set continue_on_error,   "Continue on error" );
  ("-I"    , Arg.String add_path, "Path for definitions files and machines" );
  ("-v", Arg.Unit (fun () -> Log.set_verbose true) , "Verbose mode" );
  ("-keep-macro-loc", Arg.Set MacroLexer.keep_macro_loc, "Keep macro locations");
  ("-x", Arg.Set Visibility.extended_sees, "Extended SEES" );
]

let _ =
  (try Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files")
   with Exit -> () );
  if !nb_errors > 0 then
    ( Printf.fprintf stderr "Error found.\n"; exit 1 )
  else
    exit 0
