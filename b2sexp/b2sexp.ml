open Blib
let continue_on_error = ref false
let out = ref stdout

let set_out s =
  out := open_out s

let run_on_file filename =
  try
    let input = open_in filename in
    let c = Parser.parse_component_from_channel ~filename input in
    Sexp.sexp_to_channel !out (Sexp.sexp_of_component c)
  with
  | Sys_error msg ->
    ( Printf.fprintf stderr "%s\n" msg;
      if not !continue_on_error then exit(1) )
  | Error.Fatal ->
    if not !continue_on_error then exit(1)

let add_path s =
  try File.add_path s
  with Error.Fatal -> ()

let args = [
  ("-c", Arg.Set continue_on_error, "Continue on error" );
  ("-o", Arg.String set_out, "Output file" );
  ("-keep-macro-loc", Arg.Set MacroLexer.keep_macro_loc, "Keep macro locations");
  ("-I", Arg.String add_path, "Search directory for definitions files" );
]

let _ = Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files")
