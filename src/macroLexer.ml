open Lexing_Utils
open Grammar
open Utils

module LList : sig
  type 'a t
  val break: 'a t -> ('a * 'a t) option
  val length : 'a t -> int
  val empty : 'a t
  val cons : 'a -> 'a t -> 'a t
end = struct
  type 'a t = { lst:'a list; len:int }
  let break x = match x.lst with
    | [] -> None
    | hd::tl -> Some(hd,{lst=tl;len=x.len-1})
  let length x = x.len
  let empty = { lst=[]; len=0 }
  let cons hd tl = { lst=hd::tl.lst; len=tl.len+1 }
end

module TreeLexer : sig
  type state
  val mk_state: Base_Lexer.state -> state
  val get_token_exn: state -> t_token
  val push_tree_exn: Utils.loc -> state -> tree -> unit
  val preview_token_exn:state -> t_token
end = struct

  type state = {
    lex_state:Base_Lexer.state;
    mutable stack: tree LList.t
  }

  let mk_state (s:Base_Lexer.state) : state =
    { lex_state=s; stack= LList.empty }

  let rec get_token_exn (s:state) : t_token =
    match LList.break s.stack with
    | None -> Base_Lexer.get_token_exn s.lex_state
    | Some (tree,stack_tl) ->
      begin match tree_pop tree with
        | None -> ( s.stack <- stack_tl; get_token_exn s )
        | Some (tk,tree) -> ( s.stack <- LList.cons tree stack_tl; tk )
      end

  let push_tree_exn (lc:Utils.loc) (s:state) (tree:tree) : unit =
    if LList.length s.stack < 15 then
      s.stack <- LList.cons tree s.stack
    else
      Error.raise_exn lc "Max definition depth reached (15)."

  let rec preview_token_exn (s:state): t_token =
    match LList.break s.stack with
    | None ->
      let tk = Base_Lexer.get_token_exn s.lex_state in
      ( push_tree_exn Utils.dloc s (token_to_tree tk); tk )
    | Some (tree,stack_tl) ->
      begin match tree_top tree with
        | None -> ( s.stack <- stack_tl; preview_token_exn s )
        | Some tk -> tk
      end

end

type state = {
  xstate: TreeLexer.state;
  macros: MacroTable.t;
}

let mk_state ~filename (lb:Lexing.lexbuf) (macros:MacroTable.t) : state =
    { xstate = TreeLexer.mk_state (Base_Lexer.mk_state ~filename lb);
      macros }

let merge_exn lst1 lst2 =
  let rec loop lst1 lst2 =
    match lst1, lst2 with
    | [], [] -> []
    | hd1::tl1, hd2::tl2 -> (hd1,hd2)::(loop tl1 tl2)
    | _, _ -> raise (Failure "")
  in
  try Some (loop lst1 lst2)
  with Failure _ -> None

let read_until_next_clause (s:TreeLexer.state) : t_token =
  let rec loop nb_while = match TreeLexer.get_token_exn s with
  | MACHINE, _, _ | REFINEMENT, _, _ | IMPLEMENTATION, _, _ | REFINES, _, _
  | DEFINITIONS, _, _ | IMPORTS, _, _ | SEES, _, _ | INCLUDES, _, _ | USES, _, _
  | EXTENDS, _, _ | PROMOTES, _, _ | SETS, _, _ | CONSTRAINTS, _, _
  | ABSTRACT_CONSTANTS, _, _ | CONCRETE_CONSTANTS, _, _ | CONSTANTS, _, _
  | VALUES, _, _ | ABSTRACT_VARIABLES, _, _ | VARIABLES, _, _ | CONCRETE_VARIABLES, _, _
  | ASSERTIONS, _, _ | INITIALISATION, _, _ | OPERATIONS, _, _
  | LOCAL_OPERATIONS, _, _ | EOF, _, _ | PROPERTIES, _, _ as tk -> tk
  | INVARIANT, _, _ as tk -> if nb_while > 0 then loop (nb_while-1) else tk
  | END, _, _ as tk ->
    begin match TreeLexer.preview_token_exn s with
      | EOF,_,_ -> tk
      | _ -> loop nb_while
    end
  | WHILE, _, _ -> loop (nb_while+1)
  | _ -> loop nb_while
  in
  loop 0

let rec get_token_exn (s:state) : t_token =
  match TreeLexer.get_token_exn s.xstate with
  | IDENT id, st, _ as tk ->
    begin match MacroTable.get_macro s.macros id with
      | None -> tk
      | Some (_,args,tks) ->
        let params = match args with
          | [] -> []
          | _ ->
            let lst = get_params s in
            begin match merge_exn args lst with
             | Some params -> params
             | None ->
               let n1 = List.length lst in
               let n2 = List.length args in
               Error.raise_exn st
                 ("Instanciation of macro '"^id^"' with "^string_of_int n1^
                  " paramter(s). Expecting "^string_of_int n2^" parameter(s).")
            end
        in
        let tree = tree_subst params tks in
        let () = TreeLexer.push_tree_exn st s.xstate tree in
        get_token_exn s
    end
  | DEFINITIONS, _, _ -> read_until_next_clause s.xstate
  | tk -> tk

and get_params (s:state) : t_token list list =
    match get_token_exn s with
    | LPAR, _, _ -> List.rev (read_params_exn s 0 0 [] [])
    | EOF, st, _ -> Error.raise_exn st "Unexpected end of file."
    | _, st, _ -> Error.raise_exn st "Missing definition parameters."

and read_params_exn (s:state) (nb_lpar:int) (nb_lbra:int) (rev_defs:t_token list list) (tks:t_token list) : t_token list list =
    match get_token_exn s with
    | RPAR, _, _ as next ->
      if nb_lpar = 0 && nb_lbra = 0 then
        (List.rev tks)::rev_defs (* End of rec calls *)
      else
        read_params_exn s (max (nb_lpar-1) 0) nb_lbra rev_defs (next::tks)
    | COMMA, _, _ as next ->
      if nb_lpar = 0 && nb_lbra = 0 then
        read_params_exn s 0 0 ((List.rev tks)::rev_defs) []
      else read_params_exn s nb_lpar nb_lbra rev_defs (next::tks)
    | LPAR, _, _ as next ->
      read_params_exn s (nb_lpar+1) nb_lbra rev_defs (next::tks)
    | LBRA, _, _ as next ->
      read_params_exn s nb_lpar (nb_lbra+1) rev_defs (next::tks)
    | RBRA, _, _ as next ->
      read_params_exn s nb_lpar (max (nb_lbra-1) 0) rev_defs (next::tks)
    | EOF, st, _ -> Error.raise_exn st "Unexpected end of file."
    | next -> read_params_exn s nb_lpar nb_lbra rev_defs (next::tks)
