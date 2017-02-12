open Utils
open Grammar
open Lexer_with_lk

exception Error of (loc*string)

(* Include directories for definition files *)

let paths = ref []

let add_path (p:string) : unit =
  paths := p :: !paths

let get_full_name (fn:string) : string option =
  let rec aux = function
    | [] -> None
    | p::lst ->
      let full = p ^ "/" ^ fn in
      if Sys.file_exists full then Some full
      else aux lst
  in
  aux ("." :: !paths)

let opened_def_files = ref []
let reset_opened_def_files () = opened_def_files := []

let load_def_file (lc:loc) (fn:string) : in_channel =
  ( if List.mem fn !opened_def_files then
      raise (Error (lc,"Error: trying to load '" ^ fn ^ "' twice."))
    else opened_def_files := fn :: !opened_def_files );
  match get_full_name fn with
  | Some fn ->
    begin
      try open_in fn
      with Sys_error _ -> raise (Error (lc,"Error: cannot open file '"^fn^"'."))
    end
  | None -> raise (Error (lc,"Error: cannot find file '"^fn^"'."))

(* ***** *)

type macro_table = (string,loc*ident list*t_token list) Hashtbl.t
type macro = ( ident * ident list * t_token list )

let find (hsh:macro_table) (id:string) =
  try
    let (lc,params,body) = Hashtbl.find hsh id in
    Some ((lc,id),params,body)
  with Not_found -> None

let dump_table (defs:macro_table) : unit =
  let rec concat sep = function
    | [] -> ""
    | [(_,s)] -> s
    | (_,s)::tl ->  s ^ "," ^ (concat sep tl)
  in
  let aux name (loc,params,tokens) =
    Printf.fprintf stdout "DEFINITIONS %s(%s) == (...)\n"
      name (concat "," params)
  in
  Printf.fprintf stdout ">>> DefTable\n";
  Hashtbl.iter aux defs;
  Printf.fprintf stdout "<<< DefTable\n"


let raise_err (loc:loc) (tk:token) =
  raise (Error (loc,"Error in clause DEFINITIONS: unexpected token '"
                    ^ token_to_string tk ^ "'."))

let is_def_sep state =
  let queue = Queue.create () in
  let rec aux () =
    let next = get_next state in
    let _ = Queue.add next queue in
    match next with
    | SEMICOLON, _, _ | MACHINE, _, _ | REFINEMENT, _, _
    | IMPLEMENTATION, _, _ | REFINES, _, _ | DEFINITIONS, _, _
    | IMPORTS, _, _ | SEES, _, _ | INCLUDES, _, _ | USES, _, _
    | EXTENDS, _, _ | PROMOTES, _, _ | SETS, _, _
    | ABSTRACT_CONSTANTS, _, _ | CONCRETE_CONSTANTS, _, _
    | CONSTANTS, _, _ | VALUES, _, _ | ABSTRACT_VARIABLES, _, _
    | VARIABLES, _, _ | CONCRETE_VARIABLES, _, _ | INVARIANT, _, _
    | ASSERTIONS, _, _ | INITIALISATION, _, _ | OPERATIONS, _, _
    | LOCAL_OPERATIONS, _, _ | EOF, _, _ -> false
    | EQUALEQUAL, _, _ -> true
    | _ -> aux ()
  in
  let result = aux () in
  prepend_queue state queue;
  result

let is_end_of_def_clause = function
  | REFINEMENT
  | IMPLEMENTATION
  | REFINES
  | DEFINITIONS
  | IMPORTS
  | SEES
  | INCLUDES
  | USES
  | EXTENDS
  | PROMOTES
  | SETS
  | ABSTRACT_CONSTANTS
  | CONCRETE_CONSTANTS
  | CONSTANTS
  | VALUES
  | ABSTRACT_VARIABLES
  | VARIABLES
  | CONCRETE_VARIABLES
  | INVARIANT
  | ASSERTIONS
  | INITIALISATION
  | OPERATIONS
  | LOCAL_OPERATIONS
  | PROPERTIES
  | EOF -> true
  | _ -> false

let rec state_1_start (state:state) (def_lst:macro list) : macro list =
  match get_next state with
  | DEF_FILE fn, st, _ ->
    let def_lst = parse_def_file def_lst st fn in
    state_8_def_file state def_lst
  | IDENT id, lc, _   -> state_2_eqeq_or_lpar state def_lst (lc,id)
  | tk, st, _ ->
    if is_end_of_def_clause tk then def_lst
    else raise_err st tk

and state_2_eqeq_or_lpar (state:state) (def_lst:macro list) (def_name:ident) : macro list =
  match get_next state with
  | EQUALEQUAL, _, _ -> state_3_body state def_lst def_name [] []
  | LPAR, _, _ -> state_4_param_lst state def_lst def_name
  | tk, st, _ -> raise_err st tk

and state_3_body state (def_lst:macro list) (def_name:ident) (plst_rev:ident list) (tks_rev:t_token list) : macro list =
  match get_next state with
  (* may be a separator *)
  | SEMICOLON, _, _ as next ->
    if is_def_sep state then
      begin
        let params = List.rev plst_rev in
        let tokens = List.rev tks_rev in
        state_1_start state ((def_name,params,tokens)::def_lst)
      end
      else
        state_3_body state def_lst def_name plst_rev (next::tks_rev)
  | (tk, _, _ ) as next ->
  (* end of definition clause *)
    if is_end_of_def_clause tk then
      let params = List.rev plst_rev in
      let tokens = List.rev tks_rev in
        (def_name,params,tokens)::def_lst
    else
  (* definition body *)
    state_3_body state def_lst def_name plst_rev (next::tks_rev)

and state_4_param_lst (state:state) (def_lst:macro list) (def_name:ident) : macro list =
  match get_next state with
  | IDENT id, st, _ -> state_5_comma_or_rpar state def_lst def_name [(st,id)]
  | RPAR, _, _ -> state_7_eqeq state def_lst def_name []
  | tk, st, _ -> raise_err st tk

and state_5_comma_or_rpar (state:state) (def_lst:macro list) (def_name:ident) (plst_rev:ident list) : macro list =
  match get_next state with
  | COMMA, _, _ -> state_6_param state def_lst def_name plst_rev
  | RPAR, _, _ -> state_7_eqeq state def_lst def_name plst_rev
  | tk, st, _ -> raise_err st tk

and state_6_param (state:state) (def_lst:macro list) (def_name:ident) (plst_rev:ident list) : macro list =
  match get_next state with
  | IDENT id, lc, _ -> state_5_comma_or_rpar state def_lst def_name ((lc,id)::plst_rev)
  | tk, st, _ -> raise_err st tk

and state_7_eqeq (state:state) (def_lst:macro list) (def_name:ident) (plst_rev:ident list) : macro list =
  match get_next state with
  | EQUALEQUAL, _, _ -> state_3_body state def_lst def_name plst_rev []
  | tk, st, _ -> raise_err st tk

and parse_def_file (def_lst:macro list) (loc:loc) (fn:string) : macro list =
  let input = load_def_file loc fn in
  let state = mk_state fn input in
  match get_next state with
  | DEFINITIONS, _, _ ->  state_1_start state def_lst
  | tk, st, _ -> raise_err st tk

and state_8_def_file (state:state) (def_lst:macro list) : macro list =
  match get_next state with
  | SEMICOLON, _, _ -> state_1_start state def_lst
  | tk, st, _ ->
    if is_end_of_def_clause tk then def_lst
    else raise_err st tk

let parse_defs (state:state) : macro_table =
  let defs = state_1_start state [] in
  let hsh = Hashtbl.create 47 in
  List.iter (fun ((lc,id),params,body) ->
      Hashtbl.add hsh id (lc,params,body) ) defs;
  hsh

let mk_macro_table (fname:string) (chan:in_channel) : (macro_table,loc*string) result =
  try (
    let rec aux1 state =
      match get_next state with
      | DEFINITIONS, _, _ -> true
      | EOF, _, _ -> false
      | _ -> aux1 state
    in
    let _ = reset_opened_def_files () in
    let state = mk_state fname chan in
    if aux1 state then Ok (parse_defs state)
    else Ok (Hashtbl.create 1)
  ) with Error x -> Error x

(* **************** *)

let rec mk_assoc l1 l2 =
  match l1, l2 with
  | [] , [] -> []
  | h1::t1, h2::t2 -> (snd h1,h2)::(mk_assoc t1 t2)
  | _, _ -> raise Exit

let rec mk_assoc_safe (loc:loc) (l1:ident list) (l2:t_token list list)
  : ((string*t_token list) list,loc*string) result =
  try Ok (mk_assoc l1 l2)
  with Exit -> Error (loc,"Error while expanding a definition: incorrect number of parameters.")

let expand loc (state:state) (name,e_params,body:macro) (a_params:t_token list list) : (unit,loc*string) result =
  let queue = Queue.create () in
  match mk_assoc_safe loc e_params a_params with
  | Error e -> Error e
  | Ok params ->
    begin
      List.iter (
        function
        | IDENT id, st, ed as tk ->
          begin
            try
              let actual_p = List.assoc id params in
              List.iter (fun tk0 -> Queue.add tk0 queue) actual_p
            with
              Not_found -> Queue.add tk queue
          end
        | tk -> Queue.add tk queue
      ) body;
      prepend_queue state queue;
      Ok ()
    end

let has_parameters (_,params,_:macro) : bool = not (params = [])
