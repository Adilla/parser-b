let parse_from_string str =
  match Parser.parse_component_from_string str with
  | Ok c -> c
  | Error (_,msg) -> failwith msg

open Component

let () =
  let () = Random.self_init () in
  let comp = norm_component (Generators.gen_component (Random.get_state ())) in
  let str = Easy_format.Pretty.to_string (ef_component comp) in
  let comp2 = norm_component (parse_from_string str) in
  if component_eq comp comp2 then
    print_endline "Success"
  else
    let () = print_endline "Failure" in
    let out1 = open_out "out1.scm" in
    let out2 = open_out "out2.scm" in
    let out_str = open_out "str.mch" in
    let () = output_string out_str str in
    let () = Sexp.to_channel out1 (Sexp.sexp_of_component comp) in
    let () = Sexp.to_channel out2 (Sexp.sexp_of_component comp2) in
    ()
