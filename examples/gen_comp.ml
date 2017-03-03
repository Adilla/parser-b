let out = ref stdout

let set_out filename =
  out := open_out filename

let pretty_print c =
  Easy_format.Pretty.to_channel !out (Component.ef_component c)

(*
let pretty_print_e c =
  Easy_format.Pretty.to_channel !out (Expression.ef_expr c)
*)

let args = [ ("-o" , Arg.String set_out, "Output file" ) ]

let () =
  Arg.parse args (fun _ -> ()) ("Usage: "^ Sys.argv.(0) ^" [options]");
  Random.self_init ();
  pretty_print (Generators.gen_component (Random.get_state ()))
