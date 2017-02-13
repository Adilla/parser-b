open Utils
open Lexer_with_pp

module I = Grammar.MenhirInterpreter

let loc_from_env env : Lexing.position =
  let (start,_) = I.positions env in
  start

let rec loop (state:state) (chkp:Component.component I.checkpoint)
  : (Component.component,loc*string) result =
  match chkp with
  | I.InputNeeded env -> loop state (I.offer chkp (get_next state))
  | I.Shifting _
  | I.AboutToReduce _ -> loop state (I.resume chkp)
  | I.HandlingError env ->
    Error (loc_from_env env,"Syntax error: unexpected token '" ^ get_last_token_str state ^ "'.")
  | I.Accepted v -> Ok v
  | I.Rejected -> assert false

let parse_component (filename:string) (input:in_channel) : (Component.component,loc*string) result =
  try
    ( mk_state filename input >>= fun state ->
      loop state (Grammar.Incremental.component_eof (get_current_pos state)) )
  with
  | Component.Error (p,msg) -> Error (p,msg)
  | Lexer_base.Error (p,msg) -> Error (p,msg)
