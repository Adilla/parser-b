open Utils

module P = PSyntax
module I = Grammar.MenhirInterpreter

let rec loop_exn (state:Lexer.state) (chkp:P.component I.checkpoint) : P.component =
  match chkp with
  | I.InputNeeded env -> loop_exn state (I.offer chkp (Lexer.get_token_exn state))
  | I.Shifting _
  | I.AboutToReduce _ -> loop_exn state (I.resume chkp)
  | I.HandlingError env ->
    let tk, st, _ = Lexer.get_last_token state in
    Error.raise_exn st
      ("Syntax error: unexpected token '" ^ Lexing_Utils.token_to_string tk ^ "'.")
  | I.Accepted v -> v
  | I.Rejected -> assert false (*unreachable*)

let loop (st:Lexer.state) : P.component Error.t_result =
  try Ok (loop_exn st (Grammar.Incremental.component_eof (Lexing.dummy_pos)))
  with Error.Error err -> Error err

let parse_component_from_channel ~filename:(filename:string) (input:in_channel) : P.component Error.t_result =
  match Lexer.mk_state_from_channel filename input with
  | Ok state -> loop state
  | Error err -> Error err

let parse_component_from_string (input:string) : P.component Error.t_result =
  match Lexer.mk_state_from_string input with
  | Ok state -> loop state
  | Error err -> Error err
