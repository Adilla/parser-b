open Component

let dump str =
  let out = open_out "dump" in
  let _ = Printf.fprintf out "%s" str in
  close_out out

let dump2 c1 c2 =
  let out1 = open_out "dump1" in
  let sexp1 = Sexp.sexp_of_component c1 in
  let _ = Printf.fprintf out1 "%s" (Sexp.to_string sexp1) in
  let _ = close_out out1 in
  let out2 = open_out "dump2" in
  let sexp2 = Sexp.sexp_of_component c2 in
  let _ = Printf.fprintf out2 "%s" (Sexp.to_string sexp2) in
  close_out out2

let print_and_parse c  =
  let str = Easy_format.Pretty.to_string (Print.component_to_format c) in
  match Parser.parse_component_from_string str with
  | Ok c -> c
  | Error (_,msg) -> (dump str; failwith msg)

let nb_of_tests = 10

let () =
  let () = Random.self_init () in
  for i=1 to nb_of_tests do
    let c  = Generators.gen_component (Random.get_state ()) in
    let c2 = print_and_parse c in
    if component_eq c c2 then
      print_endline "Success"
    else
      begin
        dump2 c c2;
        failwith "Failure"
      end
  done
