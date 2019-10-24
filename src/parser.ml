module P = PSyntax
module I = Grammar.MenhirInterpreter

let rec loop_exn (state:Lexer.state) (chkp:P.component I.checkpoint) : P.component =
  match chkp with
  | I.InputNeeded _ -> loop_exn state (I.offer chkp (Lexer.get_token_exn state))
  | I.Shifting _
  | I.AboutToReduce _ -> loop_exn state (I.resume chkp)
  | I.HandlingError _ ->
    let tk, st, _ = Lexer.get_last_token state in
    Error.error st
      ("Syntax error: unexpected token '" ^ Lexing_Utils.token_to_string tk ^ "'.")
  | I.Accepted v -> v
  | I.Rejected -> assert false (*unreachable*)

let loop (st:Lexer.state) : P.component =
  loop_exn st (Grammar.Incremental.component_eof (Lexing.dummy_pos))

let parse_component_from_channel ~filename:(filename:string) (input:in_channel) : P.component =
  let state = Lexer.mk_state_from_channel filename input in
  loop state

let parse_component_from_string (input:string) : P.component =
  let state = Lexer.mk_state_from_string input in
  loop state
