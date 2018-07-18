open Utils
open Lexing_Utils
open Grammar

module L = StackLexer.Make(Base_Lexer) 

let opened_def_files = ref []
let reset_opened_def_files () = opened_def_files := []

let load_def_file_exn (lc:loc) (fn:string) : in_channel*string =
  match File.get_fullname fn with
  | Some fn ->
    begin
      ( if List.mem fn !opened_def_files then
          Error.raise_exn lc ("Error: trying to load '" ^ fn ^ "' twice.")
        else opened_def_files := fn :: !opened_def_files );
      try (open_in fn,fn)
      with Sys_error _ -> Error.raise_exn lc ("Error: cannot open file '"^fn^"'.")
    end
  | None -> Error.raise_exn lc ("Error: cannot find file '"^fn^"'.")

let load_quoted_def_file_exn (lc:loc) (fn:string) : in_channel*string =
  let dir = Filename.dirname lc.Lexing.pos_fname in
  let fn = dir ^ "/" ^ fn in
  ( if List.mem fn !opened_def_files then
      Error.raise_exn lc ("Error: trying to load '" ^ fn ^ "' twice.")
    else opened_def_files := fn :: !opened_def_files );
  try (open_in fn,fn)
  with Sys_error _ -> Error.raise_exn lc ("Error: cannot open file '"^fn^"'.")

(* ***** *)

type macro = loc * string * string list * t_token list
type macro_def = loc * string list * t_token list
type t = (string,macro_def) Hashtbl.t

let raise_err (loc:loc) (tk:Grammar.token) =
  Error.raise_exn loc ("Error in clause DEFINITIONS: unexpected token '" ^ token_to_string tk ^ "'.")

let print out (defs:t) : unit =
  let aux name (_,params,tokens) =
    Printf.fprintf out "\n%s(%s) ==" name (String.concat "," params);
    List.iter (fun (tk,_,_) -> Printf.fprintf out " %s" (token_to_string tk)) tokens;
    Printf.fprintf out "\n"
  in
  Hashtbl.iter aux defs

  let is_def_sep_exn (s:L.state) : bool =
    let rec aux (accu:t_token list) : bool =
      let next = L.get_token_exn s in
      match next with
      | EQUALEQUAL, _, _ | DEF_FILE _, _, _ ->
        (List.iter (L.push_token s) (next::accu);true)
      | SEMICOLON, _, _ | MACHINE, _, _ | REFINEMENT, _, _
      | IMPLEMENTATION, _, _ | REFINES, _, _ | DEFINITIONS, _, _
      | IMPORTS, _, _ | SEES, _, _ | INCLUDES, _, _ | USES, _, _
      | EXTENDS, _, _ | PROMOTES, _, _ | SETS, _, _ | CONSTRAINTS, _, _
      | ABSTRACT_CONSTANTS, _, _ | CONCRETE_CONSTANTS, _, _
      | CONSTANTS, _, _ | VALUES, _, _ | ABSTRACT_VARIABLES, _, _
      | VARIABLES, _, _ | CONCRETE_VARIABLES, _, _ | INVARIANT, _, _
      (*FIXME invariant might be part of a while construct and not the beginnig of a clause*)
      | ASSERTIONS, _, _ | INITIALISATION, _, _ | OPERATIONS, _, _
      | LOCAL_OPERATIONS, _, _ | EOF, _, _ | PROPERTIES, _, _ ->
        (List.iter (L.push_token s) (next::accu);false)
      | CONSTANT _, _, _ | E_PREFIX _, _, _ | PREDICATE _, _, _ | E_BINDER _, _, _
      | E_INFIX_125 _, _, _ | E_INFIX_160 _, _, _ | E_INFIX_170 _, _, _
      | E_INFIX_180 _, _, _ | E_INFIX_190 _, _, _ | E_INFIX_200 _, _, _
      | WHILE, _, _ | WHEN, _, _ | WHERE, _, _ | VARIANT, _, _ | VAR, _, _
      | TILDE, _, _ | THEN, _, _ | STRUCT, _, _ | SQUOTE, _, _ | SKIP, _, _
      | SELECT, _, _ | OF, _, _ | LBRA_COMP, _, _ | ELSIF, _, _ | ELSE, _, _
      | CASE_OR, _, _ | BEGIN, _, _ | END, _, _ | PRE, _, _ | ASSERT, _, _
      | CHOICE, _, _ | IF, _, _ | CASE, _, _ | OR, _, _ | EITHER, _, _ | ANY, _, _
      | LET, _, _ | BE, _, _ | DO, _, _ | IN, _, _ | CBOOL, _, _ | NOT, _, _
      | REC, _, _ | MAPLET, _, _ | LEFTARROW, _, _ | EQUIV, _, _
      | PARALLEL, _, _ | IMPLY, _, _ | AFFECTATION, _, _ | BECOMES_ELT, _, _
      | DOLLAR_ZERO, _, _ | DOT, _, _ | BAR, _, _ | LBRA, _, _
      | RBRA, _, _ | LSQU, _, _ | RSQU, _, _ | AND, _, _
      | FORALL, _, _ | EXISTS, _, _ | EQUAL, _, _ | MEMBER_OF, _, _ | MINUS, _, _
      | COMMA, _, _ | RPAR, _, _ | LPAR, _, _ | IDENT _, _, _ | STRING _ , _, _ -> aux (next::accu)
    in
    match L.preview_token_exn s with
      | DEF_FILE _, _, _ -> true
      | STRING _, _, _ -> true
      | (IDENT _, _, _) -> aux []
      | _, _, _ -> false

  let is_end_of_def_clause (s:L.state) (is_def_file:bool) : Grammar.token -> bool = function
    | REFINEMENT | IMPLEMENTATION | REFINES | DEFINITIONS | IMPORTS | SEES
    | INCLUDES | USES | EXTENDS | PROMOTES | SETS | ABSTRACT_CONSTANTS
    | CONCRETE_CONSTANTS | CONSTANTS | VALUES | ABSTRACT_VARIABLES | VARIABLES
    | CONCRETE_VARIABLES | ASSERTIONS | INITIALISATION | OPERATIONS
    | LOCAL_OPERATIONS | PROPERTIES | MACHINE | CONSTRAINTS -> true
    | INVARIANT | SEMICOLON | EOF -> assert false
    | END ->
      (not is_def_file) &&
      (match L.preview_token_exn s with Grammar.EOF,_,_ -> true | _ -> false)
    | CONSTANT _ | E_PREFIX _ | PREDICATE _ | E_BINDER _ | E_INFIX_125 _
    | E_INFIX_160 _ | E_INFIX_170 _ | E_INFIX_180 _ | E_INFIX_190 _ | E_INFIX_200 _
    | WHILE | WHEN | WHERE | VARIANT | VAR | TILDE | THEN | STRUCT | SQUOTE | SKIP
    | SELECT | OF | LBRA_COMP | ELSIF | ELSE | CASE_OR | BEGIN | PRE | ASSERT
    | CHOICE | IF | CASE | OR | EITHER | ANY | LET | BE | DO | IN | CBOOL | NOT
    | REC | MAPLET | LEFTARROW | EQUIV | PARALLEL | IMPLY | AFFECTATION | BECOMES_ELT
    | DOLLAR_ZERO | DOT | BAR | LBRA | RBRA | LSQU | RSQU | AND | FORALL | EXISTS
    | EQUAL | MEMBER_OF | MINUS | COMMA | RPAR | LPAR | IDENT _ | STRING _ | EQUALEQUAL
    | DEF_FILE _ -> false

let rec state_1_start_exn (s:L.state) (is_def_file:bool) (def_lst:macro list) : macro list =
  match L.get_token_exn s with
  | STRING fn, st, _ ->
    let (input,fn) = load_quoted_def_file_exn st fn in
    let def_lst = parse_def_file_exn def_lst fn input in
    let () = close_in input in
    state_8_def_file_exn s is_def_file def_lst
  | DEF_FILE fn, st, _ ->
    let (input,fn) = load_def_file_exn st fn in
    let def_lst = parse_def_file_exn def_lst fn input in
    let () = close_in input in
    state_8_def_file_exn s is_def_file def_lst
  | SEMICOLON, _, _   -> state_1_start_exn s is_def_file def_lst
  | IDENT id, lc, _   -> state_2_eqeq_or_lpar_exn s is_def_file def_lst (lc,id)
  | EOF, _, _ -> def_lst
  | INVARIANT, _, _ -> def_lst
  | tk, st, _ ->
    if (not is_def_file) && is_end_of_def_clause s is_def_file tk then def_lst
    else raise_err st tk

and state_2_eqeq_or_lpar_exn (s:L.state) (is_def_file:bool) (def_lst:macro list) (lc,def_name:loc*string) : macro list =
  match L.get_token_exn s with
  | EQUALEQUAL, _, _ -> state_3_body_exn s is_def_file def_lst (lc,def_name) [] []
  | LPAR, _, _ -> state_4_param_lst_exn s is_def_file def_lst (lc,def_name)
  | tk, st, _ -> raise_err st tk

and state_3_body_exn (s:L.state) (is_def_file:bool) (def_lst:macro list)
    (lc,def_name:loc*string) (plst_rev:string list) (tks_rev:t_token list) : macro list =
  match L.get_token_exn s with
  (* may be a separator *)
  | SEMICOLON, _, _ as next ->
    if is_def_sep_exn s then
      begin
        let params = List.rev plst_rev in
        let tokens = List.rev tks_rev in
        state_1_start_exn s is_def_file ((lc,def_name,params,tokens)::def_lst)
      end
    else
      state_3_body_exn s is_def_file def_lst (lc,def_name) plst_rev (next::tks_rev)
  | (EOF, _, _ ) ->
    let params = List.rev plst_rev in
    let tokens = List.rev tks_rev in
    (lc,def_name,params,tokens)::def_lst
  | (INVARIANT, st, _ ) as next ->
    let rec aux : t_token list -> bool = function
      | [] -> false
      | (INVARIANT,_,_)::tl -> false
      | (WHILE,_,_)::tl -> true
      | _::tl -> aux tl
    in
    if aux tks_rev then
      state_3_body_exn s is_def_file def_lst (lc,def_name) plst_rev (next::tks_rev)
    else if is_def_file then raise_err st INVARIANT
    else
      let params = List.rev plst_rev in
      let tokens = List.rev tks_rev in
      (lc,def_name,params,tokens)::def_lst
  | (tk, st, _ ) as next ->
    (* end of definition clause *)
    if is_end_of_def_clause s is_def_file tk then
      if is_def_file then raise_err st tk
      else
        let params = List.rev plst_rev in
        let tokens = List.rev tks_rev in
        (lc,def_name,params,tokens)::def_lst
    else
      (* definition body *)
      state_3_body_exn s is_def_file def_lst (lc,def_name) plst_rev (next::tks_rev)

and state_4_param_lst_exn (s:L.state) (is_def_file:bool) (def_lst:macro list) (lc,def_name:loc*string) : macro list =
  match L.get_token_exn s with
  | IDENT id, _, _ -> state_5_comma_or_rpar_exn s is_def_file def_lst (lc,def_name) [id]
  | RPAR, _, _ -> state_7_eqeq_exn s is_def_file def_lst (lc,def_name) []
  | tk, st, _ -> raise_err st tk

and state_5_comma_or_rpar_exn (s:L.state) (is_def_file:bool) (def_lst:macro list)
    (lc,def_name:loc*string) (plst_rev:string list) : macro list =
  match L.get_token_exn s with
  | COMMA, _, _ -> state_6_param_exn s is_def_file def_lst (lc,def_name) plst_rev
  | RPAR, _, _ -> state_7_eqeq_exn s is_def_file def_lst (lc,def_name) plst_rev
  | tk, st, _ -> raise_err st tk

and state_6_param_exn (s:L.state) is_def_file (def_lst:macro list) (lc,def_name:loc*string) (plst_rev:string list) : macro list =
  match L.get_token_exn s with
  | IDENT id, _, _ -> state_5_comma_or_rpar_exn s is_def_file def_lst (lc,def_name) (id::plst_rev)
  | tk, st, _ -> raise_err st tk

and state_7_eqeq_exn (s:L.state) is_def_file (def_lst:macro list) (lc,def_name:loc*string) (plst_rev:string list) : macro list =
  match L.get_token_exn s with
  | EQUALEQUAL, _, _ -> state_3_body_exn s is_def_file def_lst (lc,def_name) plst_rev []
  | tk, st, _ -> raise_err st tk

and parse_def_file_exn (def_lst:macro list) (fn:string) (input:in_channel) : macro list =
  let s = L.mk_state (Base_Lexer.mk_state fn (Lexing.from_channel input)) in
  match L.get_token_exn s with
  | DEFINITIONS, _, _ ->  state_1_start_exn s true def_lst
  | tk, st, _ -> raise_err st tk

and state_8_def_file_exn (s:L.state) (is_def_file:bool) (def_lst:macro list) : macro list =
  match L.get_token_exn s with
  | SEMICOLON, _, _ -> state_1_start_exn s is_def_file def_lst
  | EOF, _, _ -> def_lst
  | INVARIANT, _, _ -> def_lst
  | tk, st, _ ->
    if (not is_def_file) && is_end_of_def_clause s is_def_file tk then def_lst
    else raise_err st tk

let parse_defs_exn (s:L.state) : t =
  let defs = state_1_start_exn s false [] in
  let hsh = Hashtbl.create 47 in
  List.iter (fun (lc,id,params,body) ->
      Hashtbl.add hsh id (lc,params,body)
    ) defs;
  hsh

let make (fname:string) (lb:Lexing.lexbuf) : t Error.t_result =
  let rec read_until_def_clause state =
    match Lexer_base.token lb with
    | DEFINITIONS -> true
    | EOF -> false
    | _ -> read_until_def_clause state
  in
  let () = reset_opened_def_files () in
  if read_until_def_clause lb then
    let s = L.mk_state (Base_Lexer.mk_state fname lb) in
    try Ok (parse_defs_exn s)
    with Error.Error err -> Error err
  else Ok (Hashtbl.create 1)

let get_macro (table:t) (id:string) : (Utils.loc*string list*t_token list) option =
  Hashtbl.find_opt table id
