open Grammar
open Utils

let fuel = ref 999
let set_macro_fuel i = fuel := i

module Lexer_With_Look_Ahead :
sig
  type state

  val token_to_string : Grammar.token -> string
  type t_token = Grammar.token * Lexing.position * Lexing.position
  val mk_state : string -> Lexing.lexbuf -> state
  val get_next_exn : state -> t_token
  val set_next : state -> t_token -> unit
  val get_last_token_str : state -> string
  val get_current_pos : state -> Lexing.position
  val prepend_queue : state -> t_token Queue.t -> unit
end = struct

  type t_token = token * Lexing.position * Lexing.position

  let dummy = ( EOF, Lexing.dummy_pos, Lexing.dummy_pos )

  type state = { lb:Lexing.lexbuf;
                 mutable queue: t_token Queue.t;
                 mutable last: t_token; }

  let get_token_exn lexbuf =
    let token = Lexer_base.token lexbuf in
    let startp = lexbuf.Lexing.lex_start_p
    and endp = lexbuf.Lexing.lex_curr_p in
    token, startp, endp

  let mk_state filename lb =
    lb.Lexing.lex_curr_p <- { lb.Lexing.lex_curr_p with Lexing.pos_fname = filename; };
    { lb; queue=Queue.create (); last=dummy; }

  let get_next_exn (state:state) : t_token =
    let next =
      if Queue.is_empty state.queue then get_token_exn state.lb
      else Queue.pop state.queue
    in state.last <- next; next

  let set_next (state:state) (tk:t_token) : unit =
    Queue.push tk state.queue

  let prepend_queue (state:state) (queue:t_token Queue.t) : unit =
    Queue.transfer state.queue queue;
    state.queue <- queue

  (* ----- *)

  let token_to_string = function
    | CONSTANT x -> Syntax.builtin_to_string x
    | E_PREFIX x -> Syntax.builtin_to_string x
    | PREDICATE x -> Syntax.pred_bop_to_string x
    | E_BINDER x -> Syntax.binder_to_string x
    | E_INFIX_125 x -> Syntax.builtin_to_string x
    | E_INFIX_160 x -> Syntax.builtin_to_string x
    | E_INFIX_170 x -> Syntax.builtin_to_string x
    | E_INFIX_180 x -> Syntax.builtin_to_string x
    | E_INFIX_190 x -> Syntax.builtin_to_string x
    | E_INFIX_200 x -> Syntax.builtin_to_string x
    | PROPERTIES -> "PROPERTIES"
    | OF -> "OF"
    | LBRA_COMP -> "{"
    | CONCRETE_VARIABLES -> "CONCRETE_VARIABLES"
    | ABSTRACT_CONSTANTS -> "ABSTRACT_CONSTANTS"
    | WHERE ->      "WHERE"
    | THEN ->       "THEN"
    | SELECT ->     "SELECT"
    | DEFINITIONS    -> "DEFINITIONS"
    | ELSIF          -> "ELSIF"
    | ELSE           -> "ELSE"
    | WHEN           -> "WHEN"
    | CASE_OR        -> "OR"
    | BEGIN          -> "BEGIN"
    | END            -> "END"
    | SKIP           -> "skip"
    | PRE            -> "PRE"
    | ASSERT         -> "ASSERT"
    | CHOICE         -> "CHOICE"
    | IF             -> "IF"
    | CASE           -> "CASE"
    | OR             -> "or"
    | EITHER         -> "EITHER"
    | ANY            -> "ANY"
    | LET            -> "LET"
    | BE             -> "BE"
    | VAR            -> "VAR"
    | WHILE          -> "WHILE"
    | DO             -> "DO"
    | INVARIANT      -> "INVARIANT"
    | VARIANT        -> "VARIANT"
    | MACHINE        -> "MACHINE"
    | CONSTRAINTS    -> "CONSTRAINTS"
    | SEES           -> "SEES"
    | INCLUDES       -> "INCLUDES"
    | PROMOTES       -> "PROMOTES"
    | EXTENDS        -> "EXTENDS"
    | USES           -> "USES"
    | SETS           -> "SETS"
    | CONSTANTS      -> "CONSTANTS"
    | VARIABLES      -> "VARIABLES"
    | ASSERTIONS     -> "ASSERTIONS"
    | OPERATIONS     -> "OPERATIONS"
    | LOCAL_OPERATIONS     -> "LOCAL_OPERATIONS"
    | IN             -> "IN"
    | REFINEMENT     -> "REFINEMENT"
    | REFINES        -> "REFINES"
    | IMPORTS        -> "IMPORTS"
    | VALUES         -> "VALUES"
    | IMPLEMENTATION -> "IMPLEMENTATION"
    | INITIALISATION -> "INITIALISATION"
    | CONCRETE_CONSTANTS -> "CONCRETE_CONSTANTS"
    | ABSTRACT_VARIABLES -> "ABSTRACT_VARIABLES"
    | CBOOL  -> "bool"
    | NOT     -> "not"
    | REC     -> "rec"
    | STRUCT  -> "struct"
    | MAPLET          -> "|->"
    | LEFTARROW       -> "<--"
    | EQUIV           -> "<=>"
    | PARALLEL        -> "||"
    | IMPLY           -> "=>"
    | AFFECTATION     -> ":="
    | BECOMES_ELT     -> "::"
    | EQUALEQUAL      -> "=="
    | DOLLAR_ZERO     -> "$0"
    | DOT             -> "."
    | SQUOTE          -> "\'"
    | BAR             -> "|"
    | LBRA            -> "{"
    | RBRA            -> "}"
    | TILDE           -> "~"
    | SEMICOLON       -> ";"
    | LSQU            -> "["
    | RSQU            -> "]"
    | AND             -> "&"
    | FORALL          -> "!"
    | EXISTS          -> "#"
    | EQUAL           -> "="
    | MEMBER_OF       -> ":"
    | MINUS           -> "-"
    | COMMA           -> ","
    | RPAR            -> ")"
    | LPAR            -> "("
    | DEF_FILE id -> Printf.sprintf "<%s>" id
    | IDENT id -> Printf.sprintf "identifier(%s)" id
    | EOF -> "__EOF__"
    | STRING s -> Printf.sprintf "\"%s\"" s

  let get_last_token_str state =
    let (tk,_,_) = state.last in
    token_to_string tk

  let get_current_pos state = state.lb.Lexing.lex_curr_p
end

let is_next_eof (state:Lexer_With_Look_Ahead.state) : bool =
  let next = Lexer_With_Look_Ahead.get_next_exn state in
  let () = Lexer_With_Look_Ahead.set_next state next in
  match next with
  | EOF, _, _ -> true
  | _, _, _ -> false

module Preprocessing : sig
  type macro_table
  type macro

  val mk_macro_table_exn : string -> Lexing.lexbuf -> macro_table
  val dump_table : out_channel -> macro_table -> unit
  val find : macro_table -> string -> macro option

  val has_parameters : macro -> bool
  val expand_exn : Utils.loc -> Lexer_With_Look_Ahead.state -> macro -> Lexer_With_Look_Ahead.t_token list list -> unit
end = struct
  open Lexer_With_Look_Ahead

  let raise_exn err_loc err_txt =
    let open Error in
    raise (Error { err_loc; err_txt })

  let opened_def_files = ref []
  let reset_opened_def_files () = opened_def_files := []

  let load_def_file_exn (lc:loc) (fn:string) : in_channel =
    match File.get_fullname fn with
    | Some fn ->
      begin
        ( if List.mem fn !opened_def_files then
            raise_exn lc ("Error: trying to load '" ^ fn ^ "' twice.")
          else opened_def_files := fn :: !opened_def_files );
        try open_in fn
        with Sys_error _ -> raise_exn lc ("Error: cannot open file '"^fn^"'.")
      end
    | None -> raise_exn lc ("Error: cannot find file '"^fn^"'.")

  let load_quoted_def_file_exn (lc:loc) (fn:string) : in_channel =
    let dir = Filename.dirname lc.Lexing.pos_fname in
    let fn = dir ^ "/" ^ fn in
    ( if List.mem fn !opened_def_files then
        raise_exn lc ("Error: trying to load '" ^ fn ^ "' twice.")
      else opened_def_files := fn :: !opened_def_files );
    try open_in fn
    with Sys_error _ -> raise_exn lc ("Error: cannot open file '"^fn^"'.")

  (* ***** *)

  type macro = loc * string * string list * t_token list
  type macro_table = (string,loc*string list*t_token list) Hashtbl.t

  let find (hsh:macro_table) (id:string) =
    try
      let (lc,params,body) = Hashtbl.find hsh id in
      Some (lc,id,params,body)
    with Not_found -> None

  let dump_table out (defs:macro_table) : unit =
    let aux name (_,params,tokens) =
      Printf.fprintf out "\n%s(%s) ==" name (String.concat "," params);
      List.iter (fun (tk,_,_) -> Printf.fprintf out " %s" (token_to_string tk)) tokens;
      Printf.fprintf out "\n"
    in
    Hashtbl.iter aux defs

  let raise_err (loc:loc) (tk:token) =
    raise_exn loc ("Error in clause DEFINITIONS: unexpected token '" ^ token_to_string tk ^ "'.")

  let is_def_sep_exn state =
    let queue = Queue.create () in
    let rec aux () =
      let next = get_next_exn state in
      let _ = Queue.add next queue in
      match next with
      | EQUALEQUAL, _, _ | DEF_FILE _, _, _ -> true

      | SEMICOLON, _, _ | MACHINE, _, _ | REFINEMENT, _, _
      | IMPLEMENTATION, _, _ | REFINES, _, _ | DEFINITIONS, _, _
      | IMPORTS, _, _ | SEES, _, _ | INCLUDES, _, _ | USES, _, _
      | EXTENDS, _, _ | PROMOTES, _, _ | SETS, _, _ | CONSTRAINTS, _, _
      | ABSTRACT_CONSTANTS, _, _ | CONCRETE_CONSTANTS, _, _
      | CONSTANTS, _, _ | VALUES, _, _ | ABSTRACT_VARIABLES, _, _
      | VARIABLES, _, _ | CONCRETE_VARIABLES, _, _ | INVARIANT, _, _
      | ASSERTIONS, _, _ | INITIALISATION, _, _ | OPERATIONS, _, _
      | LOCAL_OPERATIONS, _, _ | EOF, _, _ | PROPERTIES, _, _ -> false

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
      | COMMA, _, _ | RPAR, _, _ | LPAR, _, _ | IDENT _, _, _ | STRING _ , _, _ -> aux ()
    in
    let next = get_next_exn state in
    let _ = Queue.add next queue in
    let result =
      match next with
      | DEF_FILE _, _, _ -> true
      | STRING _, _, _ -> true
      | IDENT _, _, _ -> aux ()
      | _, _, _ -> false
    in
    prepend_queue state queue;
    result

  let is_end_of_def_clause state is_def_file = function
    | REFINEMENT | IMPLEMENTATION | REFINES | DEFINITIONS | IMPORTS | SEES
    | INCLUDES | USES | EXTENDS | PROMOTES | SETS | ABSTRACT_CONSTANTS
    | CONCRETE_CONSTANTS | CONSTANTS | VALUES | ABSTRACT_VARIABLES
    | VARIABLES | CONCRETE_VARIABLES | INVARIANT | ASSERTIONS | INITIALISATION
    | OPERATIONS | LOCAL_OPERATIONS | PROPERTIES | EOF | MACHINE | CONSTRAINTS -> true

    | END -> (not is_def_file) && is_next_eof state

    | CONSTANT _ | E_PREFIX _ | PREDICATE _ | E_BINDER _ | E_INFIX_125 _ | SEMICOLON
    | E_INFIX_160 _ | E_INFIX_170 _ | E_INFIX_180 _ | E_INFIX_190 _ | E_INFIX_200 _
    | WHILE | WHEN | WHERE | VARIANT | VAR | TILDE | THEN | STRUCT | SQUOTE | SKIP
    | SELECT | OF | LBRA_COMP | ELSIF | ELSE | CASE_OR | BEGIN | PRE | ASSERT
    | CHOICE | IF | CASE | OR | EITHER | ANY | LET | BE | DO | IN | CBOOL | NOT
    | REC | MAPLET | LEFTARROW | EQUIV | PARALLEL | IMPLY | AFFECTATION | BECOMES_ELT
    | DOLLAR_ZERO | DOT | BAR | LBRA | RBRA | LSQU | RSQU | AND | FORALL | EXISTS
    | EQUAL | MEMBER_OF | MINUS | COMMA | RPAR | LPAR | IDENT _ | STRING _ | EQUALEQUAL
    | DEF_FILE _ -> false

  let rec state_1_start_exn (state:state) (is_def_file:bool) (def_lst:macro list) : macro list =
    match get_next_exn state with
    | STRING fn, st, _ ->
      let input = load_quoted_def_file_exn st fn in
      let def_lst = parse_def_file_exn def_lst fn input in
      state_8_def_file_exn state is_def_file def_lst
    | DEF_FILE fn, st, _ ->
      let input = load_def_file_exn st fn in
      let def_lst = parse_def_file_exn def_lst fn input in
      state_8_def_file_exn state is_def_file def_lst
    | SEMICOLON, _, _   -> state_1_start_exn state is_def_file def_lst
    | IDENT id, lc, _   -> state_2_eqeq_or_lpar_exn state is_def_file def_lst (lc,id)
    | EOF, st, _ -> def_lst
    | tk, st, _ ->
      if (not is_def_file) && is_end_of_def_clause state is_def_file tk then def_lst
      else raise_err st tk

  and state_2_eqeq_or_lpar_exn (state:state) is_def_file (def_lst:macro list) (lc,def_name:loc*string) : macro list =
    match get_next_exn state with
    | EQUALEQUAL, _, _ -> state_3_body_exn state is_def_file def_lst (lc,def_name) [] []
    | LPAR, _, _ -> state_4_param_lst_exn state is_def_file def_lst (lc,def_name)
    | tk, st, _ -> raise_err st tk

  and state_3_body_exn state is_def_file (def_lst:macro list) (lc,def_name:loc*string) (plst_rev:string list) (tks_rev:t_token list) : macro list =
    match get_next_exn state with
    (* may be a separator *)
    | SEMICOLON, _, _ as next ->
      if is_def_sep_exn state then
        begin
          let params = List.rev plst_rev in
          let tokens = List.rev tks_rev in
          state_1_start_exn state is_def_file ((lc,def_name,params,tokens)::def_lst)
        end
      else
        state_3_body_exn state is_def_file def_lst (lc,def_name) plst_rev (next::tks_rev)
    | (EOF, _, _ ) ->
      let params = List.rev plst_rev in
      let tokens = List.rev tks_rev in
      (lc,def_name,params,tokens)::def_lst
    | (INVARIANT, st, _ ) as next ->
      let rec aux = function
        | [] -> false
        | (INVARIANT,_,_)::tl -> false
        | (WHILE,_,_)::tl -> true
        | _::tl -> aux tl
      in
      if aux tks_rev then
        state_3_body_exn state is_def_file def_lst (lc,def_name) plst_rev (next::tks_rev)
      else if is_def_file then raise_err st INVARIANT
      else
        let params = List.rev plst_rev in
        let tokens = List.rev tks_rev in
        (lc,def_name,params,tokens)::def_lst
    | (tk, st, _ ) as next ->
      (* end of definition clause *)
      if is_end_of_def_clause state is_def_file tk then
        if is_def_file then raise_err st tk
        else
          let params = List.rev plst_rev in
          let tokens = List.rev tks_rev in
          (lc,def_name,params,tokens)::def_lst
      else
        (* definition body *)
        state_3_body_exn state is_def_file def_lst (lc,def_name) plst_rev (next::tks_rev)

  and state_4_param_lst_exn (state:state) is_def_file (def_lst:macro list) (lc,def_name:loc*string) : macro list =
    match get_next_exn state with
    | IDENT id, _, _ -> state_5_comma_or_rpar_exn state is_def_file def_lst (lc,def_name) [id]
    | RPAR, _, _ -> state_7_eqeq_exn state is_def_file def_lst (lc,def_name) []
    | tk, st, _ -> raise_err st tk

  and state_5_comma_or_rpar_exn (state:state) is_def_file (def_lst:macro list) (lc,def_name:loc*string) (plst_rev:string list) : macro list =
    match get_next_exn state with
    | COMMA, _, _ -> state_6_param_exn state is_def_file def_lst (lc,def_name) plst_rev
    | RPAR, _, _ -> state_7_eqeq_exn state is_def_file def_lst (lc,def_name) plst_rev
    | tk, st, _ -> raise_err st tk

  and state_6_param_exn (state:state) is_def_file (def_lst:macro list) (lc,def_name:loc*string) (plst_rev:string list) : macro list =
    match get_next_exn state with
    | IDENT id, _, _ -> state_5_comma_or_rpar_exn state is_def_file def_lst (lc,def_name) (id::plst_rev)
    | tk, st, _ -> raise_err st tk

  and state_7_eqeq_exn (state:state) is_def_file (def_lst:macro list) (lc,def_name:loc*string) (plst_rev:string list) : macro list =
    match get_next_exn state with
    | EQUALEQUAL, _, _ -> state_3_body_exn state is_def_file def_lst (lc,def_name) plst_rev []
    | tk, st, _ -> raise_err st tk

  and parse_def_file_exn (def_lst:macro list) (fn:string) (input:in_channel) : macro list =
    let state = mk_state fn (Lexing.from_channel input) in
    match get_next_exn state with
    | DEFINITIONS, _, _ ->  state_1_start_exn state true def_lst
    | tk, st, _ -> raise_err st tk

  and state_8_def_file_exn (state:state) (is_def_file:bool) (def_lst:macro list) : macro list =
    match get_next_exn state with
    | SEMICOLON, _, _ -> state_1_start_exn state is_def_file def_lst
    | EOF, st, _ -> def_lst
    | tk, st, _ ->
      if (not is_def_file) && is_end_of_def_clause state is_def_file tk then def_lst
      else raise_err st tk

  let parse_defs_exn (state:state) : macro_table =
    let defs = state_1_start_exn state false [] in
    let hsh = Hashtbl.create 47 in
    List.iter (fun (lc,id,params,body) ->
        Hashtbl.add hsh id (lc,params,body) ) defs;
    hsh

  let mk_macro_table_exn (fname:string) (lb:Lexing.lexbuf) : macro_table =
    let rec aux1 state =
      match get_next_exn state with
      | DEFINITIONS, _, _ -> true
      | EOF, _, _ -> false
      | _ -> aux1 state
    in
    let () = reset_opened_def_files () in
    let state = mk_state fname lb in
    if aux1 state then parse_defs_exn state
    else Hashtbl.create 1

  (* **************** *)

  let mk_assoc_exn (loc:loc) (l1:string list) (l2:t_token list list) : (string*t_token list) list =
    let rec aux l1 l2 =
      match l1, l2 with
      | [] , [] -> []
      | h1::t1, h2::t2 -> (h1,h2)::(aux t1 t2)
      | _, _ -> raise_exn loc "Error while expanding a definition: incorrect number of parameters."
    in
    aux l1 l2

  let expand_exn loc (state:state) (lc,name,e_params,body:macro) (a_params:t_token list list) : unit =
    let queue = Queue.create () in
    let params = mk_assoc_exn loc e_params a_params in
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
    prepend_queue state queue

  let has_parameters (_,_,params,_:macro) : bool = not (params = [])
end

module Lexer_With_Preprocessing :
sig
  type state
  val get_next_exn : state -> Lexer_With_Look_Ahead.t_token (* may raise exception Lexing_base.Error *)
  val mk_state_from_channel_exn : string -> in_channel -> state
  val mk_state_from_string_exn : string -> state
  val get_current_pos : state -> Lexing.position
  val get_last_token_str : state -> string
end = struct

  type t_token = Lexer_With_Look_Ahead.t_token

  type state = { macros:Preprocessing.macro_table;
                 lstate:Lexer_With_Look_Ahead.state;
                 mutable fuel: int }

  let get_macro_table = fst

  let rec read_params_exn (nb_lpar:int) (nb_lbra:int) (state:Lexer_With_Look_Ahead.state)
      (rev_defs:(t_token list) list) (tks:t_token list) : (t_token list) list =
    match Lexer_With_Look_Ahead.get_next_exn state with
    | RPAR, _, _ as next ->
      if nb_lpar < 1 (* ie nb_lpar = 0 *) then
        (List.rev tks)::rev_defs (* End of rec calls *)
      else read_params_exn (nb_lpar-1) nb_lbra state rev_defs (next::tks)
    | COMMA, _, _ as next ->
      if nb_lpar < 1 && nb_lbra < 1 (* ie nb_lpar = nb_lbra = 0 *) then
        read_params_exn 0 0 state ((List.rev tks)::rev_defs) []
      else read_params_exn nb_lpar nb_lbra state rev_defs (next::tks)
    | LPAR, _, _ as next ->
      read_params_exn (nb_lpar+1) nb_lbra state rev_defs (next::tks)
    | LBRA, _, _ as next ->
      read_params_exn nb_lpar (nb_lbra+1) state rev_defs (next::tks)
    | RBRA, st, _ as next ->
      if nb_lbra < 1 (* ie nb_lpar = 0 *) then
        Error.raise_exn st "Unbalanced number of brackets."
      else read_params_exn nb_lpar (nb_lbra-1) state rev_defs (next::tks)
    | EOF, st, _ -> Error.raise_exn st "Unexpected end of file."
    | next -> read_params_exn nb_lpar nb_lbra state rev_defs (next::tks)

  let get_params_exn (state:Lexer_With_Look_Ahead.state) : t_token list list =
    match Lexer_With_Look_Ahead.get_next_exn state with
    | LPAR, _, _ -> List.rev (read_params_exn 0 0 state [] [])
    | EOF, st, _ -> Error.raise_exn st "Unexpected end of file."
    | tk, st, _ ->
      Error.raise_exn st
        ("Unexpected token '"^ Lexer_With_Look_Ahead.token_to_string tk ^"'.")

  let is_comp_start_exn (state:Lexer_With_Look_Ahead.state) : bool =
    let queue = Queue.create () in
    let rec aux () =
      let next = Lexer_With_Look_Ahead.get_next_exn state in
      let _ = Queue.add next queue in
      match next with
      | BAR, _, _ -> true
      | IDENT _, _, _ | COMMA, _, _ | LPAR, _, _
      | RPAR, _, _ ->  aux ()
      |  _ -> false
    in
    let result = aux () in
    Lexer_With_Look_Ahead.prepend_queue state queue;
    result

  let rec read_until_next_clause_exn state =
    match Lexer_With_Look_Ahead.get_next_exn state with
    | (MACHINE , st, ed)
    | (REFINEMENT, st, ed)
    | (IMPLEMENTATION, st, ed)
    | (REFINES, st, ed)
    | (DEFINITIONS, st, ed)
    | (IMPORTS, st, ed)
    | (SEES, st, ed)
    | (INCLUDES, st, ed)
    | (USES, st, ed)
    | (EXTENDS, st, ed)
    | (PROMOTES, st, ed)
    | (SETS, st, ed)
    | (ABSTRACT_CONSTANTS, st, ed)
    | (CONCRETE_CONSTANTS, st, ed)
    | (CONSTANTS, st, ed)
    | (VALUES, st, ed)
    | (ABSTRACT_VARIABLES, st, ed)
    | (VARIABLES, st, ed)
    | (CONCRETE_VARIABLES, st, ed)
    | (INVARIANT, st, ed)
    | (ASSERTIONS, st, ed)
    | (INITIALISATION, st, ed)
    | (OPERATIONS, st, ed)
    | (LOCAL_OPERATIONS, st, ed)
    | (CONSTRAINTS, st, ed)
    | (PROPERTIES, st, ed)
    | (EOF, st, ed) as next -> next

    | END, _, _ as next ->
      if is_next_eof state then next
      else read_until_next_clause_exn state

    | CONSTANT _, _, _ | E_PREFIX _, _, _ | PREDICATE _, _, _ | E_BINDER _, _, _
    | E_INFIX_125 _, _, _ | E_INFIX_160 _, _, _ | E_INFIX_170 _, _, _
    | E_INFIX_180 _, _, _ | E_INFIX_190 _, _, _ | E_INFIX_200 _, _, _
    | WHILE, _, _ | WHEN, _, _ | WHERE, _, _ | VARIANT, _, _ | VAR, _, _
    | TILDE, _, _ | THEN, _, _ | STRUCT, _, _ | SQUOTE, _, _ | SKIP, _, _
    | SELECT, _, _ | OF, _, _ | LBRA_COMP, _, _ | ELSIF, _, _ | ELSE, _, _
    | CASE_OR, _, _ | BEGIN, _, _ | PRE, _, _ | ASSERT, _, _
    | CHOICE, _, _ | IF, _, _ | CASE, _, _ | OR, _, _ | EITHER, _, _ | ANY, _, _
    | LET, _, _ | BE, _, _ | DO, _, _ | IN, _, _ | CBOOL, _, _ | NOT, _, _
    | REC, _, _ | MAPLET, _, _ | LEFTARROW, _, _ | EQUIV, _, _
    | PARALLEL, _, _ | IMPLY, _, _ | AFFECTATION, _, _ | BECOMES_ELT, _, _
    | DOLLAR_ZERO, _, _ | DOT, _, _ | BAR, _, _ | LBRA, _, _
    | RBRA, _, _ | LSQU, _, _ | RSQU, _, _ | AND, _, _
    | FORALL, _, _ | EXISTS, _, _ | EQUAL, _, _ | MEMBER_OF, _, _ | MINUS, _, _
    | COMMA, _, _ | RPAR, _, _ | LPAR, _, _ | IDENT _, _, _ | STRING _ , _, _
    | SEMICOLON, _, _ | DEF_FILE _, _, _ | EQUALEQUAL, _, _ ->
      read_until_next_clause_exn state

  let decr_fuel_exn lc (state:state) : unit =
    if state.fuel > 0 then
      state.fuel <- state.fuel - 1
    else
      Error.raise_exn lc ("Maximum number of definition expansions reached ("^
                          string_of_int !fuel ^
                          "). Use option -f to change this limit.")

  let get_next_exn (state:state) : t_token =
    let open Preprocessing in
    let rec aux (lstate:Lexer_With_Look_Ahead.state) : t_token =
      match Lexer_With_Look_Ahead.get_next_exn lstate with
      (* Comprehension vs Extension *)
      | (LBRA, st, ed) as next ->
        if is_comp_start_exn lstate then ( LBRA_COMP, st, ed )
        else next
      (* Macro Expansion *)
      | (IDENT id, st, ed) as next ->
        begin
          match find state.macros id with
          | None ->
            next
          | Some macro ->
            begin
              decr_fuel_exn st state; (* no more than x macro expansions for one file to avoid cyclic macro expansions *)
              if has_parameters macro then
                let params = get_params_exn lstate in
                let () = expand_exn st lstate macro params in
                aux lstate
              else
                let () = expand_exn st lstate macro [] in
                aux lstate
            end
        end
      (* Ignoring clause DEFINITIONS (it is treated in preproc) *)
      | DEFINITIONS, _, _ -> read_until_next_clause_exn lstate
      (* Next token *)
      | next -> next
    in
    aux state.lstate

  let mk_state_from_channel_exn (filename:string) (input:in_channel) : state =
    let macros = Preprocessing.mk_macro_table_exn filename (Lexing.from_channel input) in
    seek_in input 0;
    let lstate = Lexer_With_Look_Ahead.mk_state filename (Lexing.from_channel input) in
    { macros; lstate; fuel= !fuel; }

  let mk_state_from_string_exn (input:string) : state =
    let macros = Preprocessing.mk_macro_table_exn "noname" (Lexing.from_string input) in
    let lstate = Lexer_With_Look_Ahead.mk_state "noname" (Lexing.from_string input) in
    { macros; lstate; fuel= !fuel; }

  let get_last_token_str (state:state) : string =
    Lexer_With_Look_Ahead.get_last_token_str state.lstate

  let get_current_pos (state:state) =
    Lexer_With_Look_Ahead.get_current_pos state.lstate
end

type t_token = Lexer_With_Look_Ahead.t_token
type state = Lexer_With_Preprocessing.state

let get_next_exn = Lexer_With_Preprocessing.get_next_exn

let mk_state_from_channel fn inc =
  try Ok (Lexer_With_Preprocessing.mk_state_from_channel_exn fn inc)
  with Error.Error err -> Error err

let mk_state_from_string str =
  try Ok (Lexer_With_Preprocessing.mk_state_from_string_exn str)
  with Error.Error err -> Error err

let get_last_token_str = Lexer_With_Preprocessing.get_last_token_str

let get_current_pos = Lexer_With_Preprocessing.get_current_pos

type macro_table = Preprocessing.macro_table

let mk_macro_table x y =
 try Ok (Preprocessing.mk_macro_table_exn x y)
 with Error.Error err -> Error err

let print_macro_table = Preprocessing.dump_table
