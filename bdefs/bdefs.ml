open Blib
let continue_on_error = ref false
let nb_errors = ref 0

let run_on_file filename =
  try
    let input = open_in filename in
    let lb = Lexing.from_channel input in
    let mt = MacroTable.make filename lb in
    Printf.fprintf stdout "##############################################################################\n";
    Printf.fprintf stdout "### DEFINITIONS for machine %s\n" filename;
    Printf.fprintf stdout "##############################################################################\n";
    MacroTable.print stdout mt
  with
  | Sys_error msg ->
    begin
      Printf.fprintf stderr "%s\n" msg;
      incr nb_errors;
      if not !continue_on_error then raise Exit
    end
  | Error.Fatal -> 
    begin
      incr nb_errors;
      if not !continue_on_error then raise Exit
    end

let add_path s =
  try File.add_path s
  with Error.Fatal -> ()

let args = [
  ("-c", Arg.Set continue_on_error, "Continue on error" );
  ("-keep-macro-loc", Arg.Set MacroLexer.keep_macro_loc, "Keep macro locations");
  ("-I", Arg.String add_path, "Search directory for definitions files" );
]

let _ =
  (try Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files")
   with Exit -> () );
  if !nb_errors > 0 then
    ( Printf.fprintf stderr "Error found.\n"; exit 1 )
  else
    exit 0
