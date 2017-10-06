open Syntax

let dump_string i str =
  let out = open_out ("dump_test_" ^ string_of_int i) in
  let _ = Printf.fprintf out "%s" str in
  close_out out

let dump_component i c j =
  let out1 = open_out ("dump_test_" ^ string_of_int i ^ "_" ^ string_of_int j ) in
  let sexp1 = Sexp.sexp_of_component c in
  let _ = Printf.fprintf out1 "%s" (Sexp.sexp_to_string sexp1) in
  close_out out1

let print_and_parse c =
  let str = Easy_format.Pretty.to_string (Print.component_to_format c) in
  match Parser.parse_component_from_string str with
  | Ok c -> Ok c
  | Error err ->
    begin
      prerr_endline err.Error.err_txt;
      Error str
    end

let nb_of_tests = 10

let run () = 
  for i=1 to nb_of_tests do
    let c  = Generators.gen_component (Random.get_state ()) in
    match print_and_parse c with
    | Ok c2 ->
      if component_eq c c2 then
        print_endline "Success"
      else
        begin
          dump_component i c 1;
          dump_component i c2 2;
          print_endline "Failure"
        end
    | Error str ->
      begin
        dump_string i str;
        print_endline "Failure"
      end
  done

let () =
  let () = Random.self_init () in
  run ()
  
