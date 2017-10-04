let continue_on_error = ref false
let out = ref stdout

let set_out s =
  out := open_out s

let print_error (loc:Utils.loc) (msg:string) : unit =
  let open Lexing in
  Printf.fprintf stdout "[file:%s;line:%i;column:%i] %s\n"
    loc.pos_fname loc.pos_lnum (loc.pos_cnum-loc.pos_bol+1) msg;
  if not !continue_on_error then exit(1)

let run_on_file filename =
  try
    let input = open_in filename in
    match Parser.parse_component filename input with
    | Ok c -> Print.print_component !out c
    | Error (lc,msg) -> print_error lc msg
  with
  | Sys_error msg -> print_error Utils.dloc msg

let args = [
  ("-c", Arg.Set continue_on_error, "Continue on error" );
  ("-o", Arg.String set_out, "Output file" );
  ("-I", Arg.String Preprocessing.add_path, "Path for definitions files" );
]

let _ = Arg.parse args run_on_file ("Usage: "^ Sys.argv.(0) ^" [options] files")
