let print_error (loc:Utils.loc) (msg:string) : unit =
  let open Lexing in
  Printf.fprintf stdout "[file:%s;line:%i;column:%i] %s\n"
    loc.pos_fname loc.pos_lnum (loc.pos_cnum-loc.pos_bol+1) msg;
  exit(1)

let components : (string,Component.component) Hashtbl.t = Hashtbl.create 47
let views : (string,View.component_view) Hashtbl.t = Hashtbl.create 47
let backrefs : (string,Utils.ident list*(Utils.ident*View.dep_kind) list) Hashtbl.t = Hashtbl.create 47

let get_name : Component.component -> string = function
  | Component.Abstract_machine mch -> snd mch.Component.name
  | Component.Refinement ref -> snd ref.Component.name
  | Component.Implementation imp -> snd imp.Component.name

let parse_file (filename:string) : unit =
  try
    let input = open_in filename in
    match Parser.parse_component filename input with
    | Ok c -> Hashtbl.add components (get_name c) c
    | Error (lc,msg) -> print_error lc msg
  with
  | Sys_error msg -> print_error Utils.dloc msg

let args = [
  ("-I", Arg.String Preprocessing.add_path, "Path for definitions files" );
]

let get_keys (hsh:('a,'b) Hashtbl.t) : 'a list = 
  let aux a _ lst = a::lst in
  Hashtbl.fold aux hsh []

let mkdir dir =
  if not (Sys.file_exists dir) then
    ignore (Unix.mkdir dir 0o750)

let generate_html () =
  let aux name view lst =
    let is_entry =
      match view.View.component_kind with
      | View.Machine -> view.View.dependencies = []
      | _ -> false
    in
    (name,is_entry)::lst
  in
  let machines = Hashtbl.fold aux views [] in
  let _ = mkdir "./doc" in
  let _ = mkdir "./doc/machines" in
  let () = Html.generate_index "./doc" machines in
  Hashtbl.iter (Html.generate_machine_page "./doc") views

let counter = ref 0

let enter () =
  if !counter >= Hashtbl.length components then
    ( Printf.fprintf stdout "Dependency cycle detected!\n"; exit(1) )
  else
    incr counter

let leave () = decr counter

let get_backrefs (name:string) : Utils.ident list * (Utils.ident*View.dep_kind) list =
  try Hashtbl.find backrefs name
  with Not_found -> ([], [])

let get_list : ('a*'b list) option -> 'b list = function
  | None -> []
  | Some (_,lst) -> lst

let update_backrefs _ (c:Component.component) : unit =
  let open Component in
  let add_refines (name:Utils.ident) (refined_mch:Utils.ident) : unit =
    let (a,b) = get_backrefs (snd refined_mch) in
    Hashtbl.add backrefs (snd refined_mch) (name::a,b)
  in
  let add_required_by (name:Utils.ident) (dk:View.dep_kind) (required_mch:Utils.ident) : unit =
    let (a,b) = get_backrefs (snd required_mch) in
    Hashtbl.add backrefs (snd required_mch) (a,(name,dk)::b)
  in
  match c with
   | Abstract_machine mch ->
    begin
      List.iter (add_required_by mch.name View.D_Sees) (get_list mch.clause_sees);
      List.iter (add_required_by mch.name View.D_Uses) (get_list mch.clause_uses);
      List.iter (add_required_by mch.name View.D_Includes) (List.map fst (get_list mch.clause_includes));
      List.iter (add_required_by mch.name View.D_Includes) (List.map fst (get_list mch.clause_extends))
    end
   | Refinement ref ->
    begin
      add_refines ref.name ref.refines;
      List.iter (add_required_by ref.name View.D_Sees) (get_list ref.clause_sees);
      List.iter (add_required_by ref.name View.D_Includes) (List.map fst (get_list ref.clause_includes));
      List.iter (add_required_by ref.name View.D_Includes) (List.map fst (get_list ref.clause_extends))
    end
   | Implementation imp ->
     begin
       add_refines imp.name imp.refines;
       List.iter (add_required_by imp.name View.D_Sees) (get_list imp.clause_sees);
       List.iter (add_required_by imp.name View.D_Imports) (List.map fst (get_list imp.clause_imports));
       List.iter (add_required_by imp.name View.D_Imports) (List.map fst (get_list imp.clause_extends_B0))
     end

let _ =
  let () = Arg.parse args parse_file ("Usage: "^ Sys.argv.(0) ^" [options] files") in
  let rec component_to_view (name:string) : View.component_view option =
    if Hashtbl.mem views name then Some (Hashtbl.find views name)
    else if Hashtbl.mem components name then
      let _ = enter () in
      let (refined_by,required_by) = get_backrefs name in
      let view = View.make component_to_view refined_by required_by (Hashtbl.find components name) in
      let _ = leave () in
      let () = Hashtbl.add views name view in
      Some view
    else
      None
  in
  Hashtbl.iter update_backrefs components;
  List.iter (fun name -> ignore (component_to_view name)) (get_keys components);
  generate_html ()
