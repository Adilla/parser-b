open Blib
module T = TSyntax
let continue_on_error = ref false

type machine_interface = Global.t_interface
type t_item = Done of T.component*machine_interface option | InProgress

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

let out_dir = ref "."

let open_rs (pkg_name:string) =
  open_out (!out_dir ^ "/"^ pkg_name ^".rs") (*FIXME*)

let rec get_pkg_name (cp:T.component) : SyntaxCore.lident =
  let aux (name:string) =
    match File.get_fullname_comp name with
    | None -> assert false
    | Some fn ->
      begin match Hashtbl.find_opt ht fn with
      | Some (Done (c,_)) -> get_pkg_name c
      | _ -> assert false
      end
  in
  match cp.T.co_desc with
  | T.Machine _ ->  cp.T.co_name
  | T.Refinement ref -> aux ref.T.ref_refines.SyntaxCore.lid_str
  | T.Implementation imp -> aux imp.T.imp_refines.SyntaxCore.lid_str

let run_on_file (filename:string) : unit = (*FIXME bind*)
  let () = Log.write "Processing file '%s'...\n%!" filename in
  match type_component_from_filename ht filename with
  | Error err -> print_error err
  | Ok (cp,_) ->
    let pkg_name = get_pkg_name cp in
    begin match Codegen.to_package pkg_name cp with
      | Error err -> print_error err
      | Ok pkg ->
        let rs = open_rs pkg_name.lid_str in
        begin match Rustprint.print_package rs pkg with
          | Ok () -> ()
          | Error err -> print_error err
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
  ("-o", Arg.Set_string out_dir, "Set output directory" );
]

let _ = Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files")
