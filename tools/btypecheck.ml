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

let rec type_component_from_filename (ht:interface_table) (filename:string) : machine_interface Error.t_result =
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
        begin match Parser.parse_component filename input with
          | Ok c ->
            let () = close_in input in
            let () = Log.write "Typing file '%s'...\n%!" filename in
            let () = Hashtbl.add ht filename InProgress in
            begin match Typechecker.get_interface (f ht) c with
              | Ok (_,itf) ->
                let () = Hashtbl.add ht filename (Done itf) in
                Ok itf
              | Error _ as err -> err
            end
          | Error _ as err -> err
        end
    end

and f (ht:interface_table) (mch_loc:Utils.loc) (mch_name:string) : machine_interface option =
  match File.get_fullname_comp mch_name with
  | None ->
    let err = { Error.err_loc=mch_loc; err_txt="Cannot find machine '"^mch_name^"'." } in
    ( Error.print_error err; None )
  | Some fn ->
   begin match type_component_from_filename ht fn with
     | Ok ok -> Some ok
     | Error err -> ( print_error err; None )
   end

let ht:interface_table = Hashtbl.create 47

let run_on_file (filename:string) : unit =
  let () = Log.write "Processing file '%s'...\n%!" filename in
  match type_component_from_filename ht filename with
  | Error err -> print_error err
  | Ok _ -> ()

let add_path x =
  match File.add_path x with
  | Ok _ -> ()
  | Error err -> print_error_no_loc err

let set_alstm_opt () =
  Typechecker.allow_becomes_such_that_in_implementation := true;
  Typechecker.allow_out_parameters_in_precondition := true;
  Global.set_extended_sees true

let args = [
  ("-c"    , Arg.Set continue_on_error,   "Continue on error" );
  ("-I", Arg.String add_path, "Path for definitions files" );
  ("-v", Arg.Unit (fun () -> Log.set_verbose true) , "Verbose mode" );
  ("-x", Arg.Unit set_alstm_opt, "(no documentation)" );
  ("-f", Arg.Int Lexer.set_macro_fuel, "Max number of definition expansions for one file (default is 999)." );
]

let _ = Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files")
