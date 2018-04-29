(** Lexer with a stack buffer*)
open Lexing_Utils

module Make (Lexer: sig
    type state
    val get_token_exn: state -> t_token
  end) :
sig
  type state
  val mk_state: Lexer.state -> state
  val get_token_exn: state -> t_token
  val preview_token_exn:state -> t_token
  val push_token: state -> t_token -> unit
end
