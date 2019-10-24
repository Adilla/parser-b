let continue_on_error = ref false
let out = ref stdout

let set_out s =
  out := open_out s

let run_on_file filename =
  try
    let input = open_in filename in
    let c = Blib.Parser.parse_component_from_channel ~filename input in
    Blib.Print.print_component !out c
  with
  | Sys_error msg ->
    ( Printf.fprintf stderr "%s\n" msg;
      if not !continue_on_error then exit(1) )
  | Blib.Error.Fatal -> if not !continue_on_error then exit(1)

let add_path s =
  try Blib.File.add_path s
  with Blib.Error.Fatal -> if not !continue_on_error then exit(1) 

let args = [
  ("-c", Arg.Set continue_on_error, "Continue on error" );
  ("-o", Arg.String set_out, "Output file" );
  ("-keep-macro-loc", Arg.Set Blib.MacroLexer.keep_macro_loc, "Keep macro locations");
  ("-I", Arg.String add_path, "Search directory for definitions files" );
]

let _ = Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files")
