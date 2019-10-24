(** Main lexer: macro expansions and bracket disambiguation*)
open Lexing_Utils

type state

val mk_state_from_channel : string -> in_channel -> state
val mk_state_from_string : string -> state

val get_token_exn : state -> t_token (* may raise exception Error.Error *)
val get_last_token : state -> t_token
