open Component

let print_and_parse (c:component) : component =
  let str = Easy_format.Pretty.to_string (Print.ef_component c) in
  match Parser.parse_component_from_string str with
  | Ok c -> c
  | Error (_,msg) -> failwith msg

let nb_of_tests = 10

let () =
  let () = Random.self_init () in
  for i=1 to nb_of_tests do
    let c  = norm_component (Generators.gen_component (Random.get_state ())) in
    let c2 = norm_component (print_and_parse c) in
    if component_eq c c2 then
      print_endline "Success"
    else
      failwith "Failure"
  done
