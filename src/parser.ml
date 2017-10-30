open Utils
open Syntax

module I = Grammar.MenhirInterpreter

let loc_from_env env : Lexing.position =
  let (start,_) = I.positions env in
  start

let rec loop_exn (state:Lexer.state) (chkp:p_component I.checkpoint) : p_component =
  match chkp with
  | I.InputNeeded env -> loop_exn state (I.offer chkp (Lexer.get_next_exn state))
  | I.Shifting _
  | I.AboutToReduce _ -> loop_exn state (I.resume chkp)
  | I.HandlingError env ->
    Error.raise_exn (loc_from_env env) ("Syntax error: unexpected token '"
                                        ^ Lexer.get_last_token_str state ^ "'.")
  | I.Accepted v -> v
  | I.Rejected -> assert false (*unreachable*)

let loop (st:Lexer.state) : p_component Error.t_result =
  try Ok (loop_exn st (Grammar.Incremental.component_eof (Lexer.get_current_pos st)))
  with Error.Error err -> Error err

let parse_component (filename:string) (input:in_channel) : p_component Error.t_result =
  match Lexer.mk_state_from_channel filename input with
  | Ok state -> loop state
  | Error err -> Error err

let parse_component_from_string (input:string) : p_component Error.t_result =
  match Lexer.mk_state_from_string input with
  | Ok state -> loop state
  | Error err -> Error err
