open Utils
open Grammar

type t_token = Lexer_with_lk.t_token

type state = { macros:Preprocessing.macro_table;
               lstate:Lexer_with_lk.state;
               mutable fuel: int }

let get_macro_table = fst

exception Error of (loc*string)

let rec read_params (nb_lpar:int) (nb_lbra:int) (state:Lexer_with_lk.state)
    (rev_defs:(t_token list) list) (tks:t_token list) : (t_token list) list =
  match Lexer_with_lk.get_next state with
  | RPAR, _, _ as next ->
    if nb_lpar < 1 (* ie nb_lpar = 0 *) then
      (List.rev tks)::rev_defs (* End of rec calls *)
    else read_params (nb_lpar-1) nb_lbra state rev_defs (next::tks)
  | COMMA, _, _ as next ->
    if nb_lpar < 1 && nb_lbra < 1 (* ie nb_lpar = nb_lbra = 0 *) then
      read_params 0 0 state ((List.rev tks)::rev_defs) []
    else read_params nb_lpar nb_lbra state rev_defs (next::tks)
  | LPAR, _, _ as next ->
    read_params (nb_lpar+1) nb_lbra state rev_defs (next::tks)
  | LBRA, _, _ as next ->
    read_params nb_lpar (nb_lbra+1) state rev_defs (next::tks)
  | RBRA, st, _ as next ->
    if nb_lbra < 1 (* ie nb_lpar = 0 *) then
      raise (Error (st,"Unbalanced number of brackets."))
    else read_params nb_lpar (nb_lbra-1) state rev_defs (next::tks)
  | EOF, st, _ -> raise (Error (st,"Unexpected end of file."))
  | next -> read_params nb_lpar nb_lbra state rev_defs (next::tks)

let get_params (state:Lexer_with_lk.state) : t_token list list =
  match Lexer_with_lk.get_next state with
  | LPAR, _, _ -> List.rev (read_params 0 0 state [] [])
  | EOF, st, _ -> raise (Error (st,"Unexpected end of file."))
  | tk, st, _ -> raise (Error (st,"Unexpected token '"^ Lexer_with_lk.token_to_string tk ^"'."))

let is_comp_start (state:Lexer_with_lk.state) : bool =
  let queue = Queue.create () in
  let rec aux () =
    let next = Lexer_with_lk.get_next state in
    let _ = Queue.add next queue in
    match next with
    | BAR, _, _ -> true
    | IDENT _, _, _ | COMMA, _, _ | LPAR, _, _
    | RPAR, _, _ ->  aux ()
    |  _ -> false
  in
  let result = aux () in
  Lexer_with_lk.prepend_queue state queue;
  result

let rec read_until_next_clause state =
  match Lexer_with_lk.get_next state with
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
  | (EOF, st, ed) as next -> next
  | _ -> read_until_next_clause state

let decr_fuel (state:state) : unit =
  if state.fuel > 0 then
    state.fuel <- state.fuel - 1
  else
    failwith "Cyclic macro detected."

let get_next (state:state) : t_token =
  let open Preprocessing in
  let rec aux (lstate:Lexer_with_lk.state) : t_token =
    match Lexer_with_lk.get_next lstate with
    (* Comprehension vs Extension *)
    | (LBRA, st, ed) as next ->
      if is_comp_start lstate then ( LBRA_COMP, st, ed )
      else next
    (* Macro Expansion *)
    | (IDENT id, st, ed) as next ->
      begin
        match find state.macros id with
        | None ->
          next
        | Some macro ->
          begin
            decr_fuel state; (* no more than x macro expansions for one file to avoid cyclic macro expansions *)
            if has_parameters macro then
              let params = get_params lstate in
              let _ = expand st lstate macro params in
              aux lstate
            else
              let _ = expand st lstate macro [] in
              aux lstate
          end
      end
    (* Ignoring clause DEFINITIONS (it is treated in preproc) *)
    | DEFINITIONS, _, _ -> read_until_next_clause lstate
    (* Next token *)
    | next -> next
  in
  aux state.lstate

let mk_state (filename:string) (input:in_channel) : (state,loc*string) result =
  match Preprocessing.mk_macro_table filename input with
  | Ok macros ->
    seek_in input 0;
    let lstate = Lexer_with_lk.mk_state filename input in
    Ok { macros; lstate; fuel=999; }
  | Error e -> Error e

let get_last_token_str (state:state) : string =
  Lexer_with_lk.get_last_token_str state.lstate

let get_current_pos (state:state) =
  Lexer_with_lk.get_current_pos state.lstate
