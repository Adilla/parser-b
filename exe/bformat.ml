let continue_on_error = ref false
let out = ref stdout

let set_out s =
  out := open_out s

let print_error err =
  Blib.Error.print_error err;
  if not !continue_on_error then exit(1)

let print_error_no_loc msg =
  Printf.fprintf stderr "%s\n" msg;
  if not !continue_on_error then exit(1)

let run_on_file filename =
  try
    let input = open_in filename in
    match Blib.Parser.parse_component_from_channel ~filename input with
    | Ok c -> Blib.Print.print_component !out c
    | Error err -> print_error err
  with
  | Sys_error msg -> print_error_no_loc msg

let add_path s =
  match Blib.File.add_path s with
  | Ok _ -> ()
  | Error err -> print_error_no_loc err

let args = [
  ("-c", Arg.Set continue_on_error, "Continue on error" );
  ("-o", Arg.String set_out, "Output file" );
  ("-keep-macro-loc", Arg.Set Blib.MacroLexer.keep_macro_loc, "Keep macro locations");
  ("-I", Arg.String add_path, "Search directory for definitions files" );
]

let _ = Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files")
