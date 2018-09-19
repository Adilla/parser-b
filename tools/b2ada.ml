module T = TSyntax
let continue_on_error = ref false

type machine_interface = Global.t_interface
type t_item = Done of TSyntax.component*machine_interface option | InProgress

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

let rec type_component_from_filename (ht:interface_table) (filename:string)
  : (T.component*machine_interface option) Error.t_result =
  match safe_find ht filename with
  | Some (Done (cp,itf)) -> Ok (cp,itf)
  | Some InProgress ->
    Error { Error.err_loc=Utils.dloc;
            Error.err_txt="Error: dependency cycle detected." }
  | None ->
    begin match open_in filename with
      | None -> Error { Error.err_loc=Utils.dloc;
                        Error.err_txt="Cannot find file '"^filename^"'." }
      | Some input ->
        let () = Log.write "Parsing file '%s'...\n%!" filename in
        begin match Parser.parse_component_from_channel ~filename input with
          | Ok c ->
            let () = close_in input in
            let () = Log.write "Typing file '%s'...\n%!" filename in
            let () = Hashtbl.add ht filename InProgress in
            begin match Typechecker.type_component (f ht) c with
              | Ok (cp,itf) ->
                let () = Hashtbl.add ht filename (Done (cp,itf)) in
                Ok (cp,itf)
              | Error _ as err -> err
            end
          | Error _ as err -> err
        end
    end

and f (ht:interface_table) (mch_loc:Utils.loc) (mch_name:string) : machine_interface option =
  match File.get_fullname_comp mch_name with
  | None ->
    let err = { Error.err_loc=mch_loc; err_txt="Cannot find the machine '"^mch_name^"'." }
    in ( Error.print_error err; None )
  | Some fn ->
   begin match type_component_from_filename ht fn with
     | Ok (_,Some itf) -> Some itf
     | Ok (_,None) -> assert false (*FIXME*)
     | Error err -> ( Error.print_error err; None )
   end

let ht:interface_table = Hashtbl.create 47

let open_ads (pkg_name:Codegen.Ada_ident.t_pkg_id) =
  open_out ("./"^Codegen.Ada_ident.pkg_to_string pkg_name^".ads") (*FIXME*)

let open_adb (pkg_name:Codegen.Ada_ident.t_pkg_id) =
  open_out ("./"^Codegen.Ada_ident.pkg_to_string pkg_name^".adb") (*FIXME*)

let rec get_pkg_name (cp:T.component) : Codegen.Ada_ident.t_pkg_id Error.t_result =
  let aux (name:string) : Codegen.Ada_ident.t_pkg_id Error.t_result =
    match File.get_fullname_comp name with
    | None -> assert false
    | Some fn ->
      begin match Hashtbl.find_opt ht fn with
      | Some (Done (c,_)) -> get_pkg_name c
      | _ -> assert false
      end
  in
  match cp.T.co_desc with
  | T.Machine _ ->
    begin match Codegen.Ada_ident.make_pkg_id cp.T.co_name.SyntaxCore.lid_str with
    | Some x -> Ok x
    | None -> Error { Error.err_loc=cp.T.co_name.SyntaxCore.lid_loc;
                      err_txt=("'"^cp.T.co_name.SyntaxCore.lid_str^
                               "' is not a valid ada identifier.") }
    end
  | T.Refinement ref -> aux ref.T.ref_refines.SyntaxCore.lid_str
  | T.Implementation imp -> aux imp.T.imp_refines.SyntaxCore.lid_str

let run_on_file (filename:string) : unit = (*FIXME bind*)
  let () = Log.write "Processing file '%s'...\n%!" filename in
  match type_component_from_filename ht filename with
  | Error err -> print_error err
  | Ok (cp,_) ->
    begin match get_pkg_name cp with
      | Error err -> print_error err
      | Ok pkg_name ->
        begin match Codegen.Ada.to_package pkg_name cp with
          | Error err -> print_error err
          | Ok pkg ->
            let ads = open_ads pkg_name in
            begin match Adaprint.print_package_spec ads pkg with
              | Ok () ->
                if not (Adaprint.is_package_body_empty pkg) then
                  let adb = open_adb pkg_name in
                  begin match Adaprint.print_package_body adb pkg with
                    | Ok () -> ()
                    | Error err -> print_error err
                  end
                else
                  let fn = "./"^Codegen.Ada_ident.pkg_to_string pkg_name^".adb" in (*FIXME*)
                  if Sys.file_exists fn then Sys.remove fn
              | Error err -> print_error err
            end
        end
    end

let add_path x =
  match File.add_path x with
  | Ok _ -> ()
  | Error err -> print_error_no_loc err

let set_alstm_opt () = (*FIXME options*)
  Typechecker.allow_becomes_such_that_in_implementation := true;
  Typechecker.allow_out_parameters_in_precondition := true;
  Visibility.extended_sees := true 

let args = [
  ("-c", Arg.Set continue_on_error,   "Continue on error" );
  ("-I", Arg.String add_path, "Path for definitions files" );
  ("-v", Arg.Unit (fun () -> Log.set_verbose true) , "Verbose mode" );
  ("-keep-macro-loc", Arg.Set MacroLexer.keep_macro_loc, "Keep macro locations");
  ("-x", Arg.Unit set_alstm_opt, "(no documentation)" );
]

let _ = Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files")
