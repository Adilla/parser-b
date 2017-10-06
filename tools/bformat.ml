let continue_on_error = ref false
let out = ref stdout
let sexp = ref false

let set_out s =
  out := open_out s

let print_error err =
  Error.print_error err;
  if not !continue_on_error then exit(1)

let print_error_no_loc msg =
  Printf.fprintf stderr "%s\n" msg;
  if not !continue_on_error then exit(1)

let pretty_print c =
  if !sexp then
    Sexp.sexp_to_channel !out (Sexp.sexp_of_component c)
  else
    Print.print_component !out c

let mk_err err_txt =
  let open Error in { err_loc=Utils.dloc; err_txt }

let run_on_file filename =
  try
    let input = open_in filename in
    match Parser.parse_component filename input with
    | Ok c -> pretty_print c
    | Error err -> print_error err
  with
  | Sys_error msg -> print_error (mk_err msg)

let add_path s =
  match File.add_path s with
  | Ok _ -> ()
  | Error err -> print_error_no_loc err

let args = [
  ("-c", Arg.Set continue_on_error, "Continue on error" );
  ("-o", Arg.String set_out, "Output file" );
  ("-sexp", Arg.Set sexp, "Output as s-expression" );
  ("-I", Arg.String add_path, "Path for definitions files" );
]

let _ = Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files")
