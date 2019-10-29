let continue_on_error = ref false
let out = ref stdout
let check_ext = ref false
let debug_mode = ref false
let nb_errors = ref 0
let tags = ref Tags.empty

let set_out file =
  let outf = open_out file in
  out := outf

let check_extension filename =
  (not !check_ext)
  || (Filename.check_suffix filename ".mch")
  || (Filename.check_suffix filename ".imp")
  || (Filename.check_suffix filename ".ref")

let debug fmt =
  if !debug_mode then Printf.kfprintf (fun _ -> prerr_newline ()) stderr fmt
  else Printf.ifprintf stderr fmt

let run_on_file filename =
  if not (check_extension filename) then
    debug "Ignoring file '%s' (unknown extension)." filename
  else
    begin
      try
        debug "Generating tags for file '%s'." filename;
        let input = open_in filename in
        let c = Blib.Parser.parse_component_from_channel ~filename input in
        tags := Tags.add_tags !tags c
      with
      | Blib.Error.Fatal ->
        begin
          incr nb_errors;
          if not !continue_on_error then raise Exit
        end
      | Sys_error msg ->
        begin
          Printf.fprintf stderr "%s\n" msg;
          incr nb_errors;
          if not !continue_on_error then raise Exit
        end
    end

let add_path s =
  try Blib.File.add_path s
  with Blib.Error.Fatal -> ()

let args = [
  ("-c"    , Arg.Set continue_on_error,   "Continue on error" );
  ("-I", Arg.String add_path, "Search directory for definitions files" );
  ("-d"    , Arg.Set debug_mode, "Print debugging informations." );
  ("-e"    , Arg.Set check_ext, "Ignoring files with extension different from .mch, .ref or .imp" );
  ("-keep-macro-loc", Arg.Set Blib.MacroLexer.keep_macro_loc, "Keep macro locations");
  ("-o"    , Arg.String set_out, "Output file (default is standard output)" )
]

let _ =
  ( try Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files")
    with Exit -> () );
  Tags.print_tags !out !tags;
  if !nb_errors > 0 then
    ( Printf.fprintf stderr "Error found.\n"; exit 1 )
  else
    exit 0
