let continue_on_error = ref false

type machine_interface = Global.t_interface
type t_item = Done of machine_interface | InProgress

let open_in (fn:string) : in_channel option =
  try Some (open_in fn) with Sys_error _ -> None

type interface_table = (string,t_item) Hashtbl.t

let safe_find ht s =
  try Some (Hashtbl.find ht s)
  with Not_found -> None

let print_error err =
  Error.print_error err;
  if not !continue_on_error then exit(1)

let print_error_no_loc msg =
  Printf.fprintf stderr "%s\n" msg;
  if not !continue_on_error then exit(1)

let rec type_component_from_filename (ht:interface_table) (filename:string) : machine_interface option =
  match safe_find ht filename with
  | Some (Done itf) -> Some itf
  | Some InProgress ->
    Error.raise_exn Utils.dloc "Error: dependency cycle detected."
  | None ->
    begin match open_in filename with
      | None -> None
      | Some input ->
        let () = Log.write "Parsing file '%s'...\n%!" filename in
        begin match Parser.parse_component filename input with
          | Ok c ->
            let () = close_in input in
            let () = Log.write "Typing file '%s'...\n%!" filename in
            let () = Hashtbl.add ht filename InProgress in
            begin match Typechecker.get_interface (f ht) c with
              | Ok itf ->
                let () = Hashtbl.add ht filename (Done itf) in
                Some itf
              | Error err -> (print_error err; None )
            end
          | Error err -> ( print_error err; None )
        end
    end

and f (ht:interface_table) (mch_name:string) : machine_interface option =
  match File.get_fullname_comp mch_name with
  | None -> None
  | Some fn -> type_component_from_filename ht fn

let ht:interface_table = Hashtbl.create 47

let run_on_file (filename:string) : unit =
  try 
    let () = Log.write "Processing file '%s'...\n%!" filename in
    match type_component_from_filename ht filename with
    | None -> print_error_no_loc ("Cannot find file '"^filename^"'.")
    | Some _ -> ()
  with
    Error.Error err -> print_error err

let add_path x =
  match File.add_path x with
  | Ok _ -> ()
  | Error err -> print_error_no_loc err

let set_alstm_opt () =
  Typechecker.allow_becomes_such_that_in_implementation := true;
  Typechecker.allow_out_parameters_in_precondition := true

let args = [
  ("-c"    , Arg.Set continue_on_error,   "Continue on error" );
  ("-I", Arg.String add_path, "Path for definitions files" );
  ("-v", Arg.Unit (fun () -> Log.set_verbose true) , "Verbose mode" );
  ("-x", Arg.Unit set_alstm_opt, "(no documentation)" );
]

let _ = Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files")
