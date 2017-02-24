let continue_on_error = ref false
let use_stdout = ref false

let error loc fmt =
    Printf.kfprintf (fun _ -> prerr_newline () (*; exit 1*) ) stderr fmt

let print_error (loc:Utils.loc) (msg:string) : unit =
  let open Lexing in
  Printf.fprintf stdout "[file:%s;line:%i;column:%i] %s\n"
    loc.pos_fname loc.pos_lnum (loc.pos_cnum-loc.pos_bol+1) msg;
  if not !continue_on_error then exit(1)

let pretty_print filename c =
  let out =
    if !use_stdout then stdout
    else open_out (filename ^ ".pp")
  in
  Easy_format.Pretty.to_channel out (Component.ef_component c)

let run_on_file filename =
  try
    let input = open_in filename in
    match Parser.parse_component filename input with
    | Ok c -> pretty_print filename c
    | Error (lc,msg) -> print_error lc msg
  with
  | Sys_error msg -> print_error Utils.dloc msg

let args = [
  ("-c"    , Arg.Set continue_on_error,   "Continue on error" );
  ("-stdout", Arg.Set use_stdout,   "Use standard output" );
  ("-I"    , Arg.String Preprocessing.add_path, "Path for definitions files" );
]

let _ = Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files")
