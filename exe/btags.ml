let continue_on_error = ref false
let out = ref stdout
let check_ext = ref false
let debug_mode = ref false
let tags = ref Tags.empty

let set_out file =
  let outf = open_out file in
  out := outf

let check_extension filename =
  (not !check_ext)
  || (Filename.check_suffix filename ".mch")
  || (Filename.check_suffix filename ".imp")
  || (Filename.check_suffix filename ".ref")

(* let continue_on_error = ref false *)

let debug fmt =
  if !debug_mode then Printf.kfprintf (fun _ -> prerr_newline ()) stderr fmt
  else Printf.ifprintf stderr fmt

let print_error err =
  Blib.Error.print_error err;
  if not !continue_on_error then exit(1)

let print_error_no_loc msg =
  Printf.fprintf stderr "%s\n" msg;
  if not !continue_on_error then exit(1)

let run_on_file filename =
  if not (check_extension filename) then
    debug "Ignoring file '%s' (unknown extension)." filename
  else
    begin
      try
        debug "Generating tags for file '%s'." filename;
        let input = open_in filename in
        match Blib.Parser.parse_component_from_channel ~filename input with
        | Ok c -> tags := Tags.add_tags !tags c
        | Error err -> print_error err
      with
      | Sys_error err_txt -> print_error { Blib.Error.err_loc=Blib.Utils.dloc; err_txt } 
    end

let add_path s =
  match Blib.File.add_path s with
  | Ok _ -> ()
  | Error err -> print_error_no_loc err

let args = [
  ("-c"    , Arg.Set continue_on_error,   "Continue on error" );
  ("-I", Arg.String add_path, "Search directory for definitions files" );
  ("-d"    , Arg.Set debug_mode, "Print debugging informations." );
  ("-e"    , Arg.Set check_ext, "Ignoring files with extension different from .mch, .ref or .imp" );
  ("-keep-macro-loc", Arg.Set Blib.MacroLexer.keep_macro_loc, "Keep macro locations");
  ("-o"    , Arg.String set_out, "Output file (default is standard output)" )
]

let _ =
  Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files");
  Tags.print_tags !out !tags
