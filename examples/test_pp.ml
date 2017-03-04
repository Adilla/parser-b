let parse_from_string str =
  match Parser.parse_component_from_string str with
  | Ok c -> c
  | Error (_,msg) -> failwith msg

let () =
  let () = Random.self_init () in
  let comp = Generators.gen_component (Random.get_state ()) in
  let str = Easy_format.Pretty.to_string (Component.ef_component comp) in
  let comp2 = parse_from_string str in
  if Component.component_eq comp comp2 then
    print_endline "Success"
  else
    let () = print_endline "Failure" in
    let out1 = open_out "out1.mch" in
    let out2 = open_out "out2.mch" in
    let () = Easy_format.Pretty.to_channel out1 (Component.ef_component comp) in
    let () = Easy_format.Pretty.to_channel out2 (Component.ef_component comp2) in
    ()
