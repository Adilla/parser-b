(** Lexer with macro expansion*)
open Lexing_Utils

type state
val mk_state: filename:string -> Lexing.lexbuf -> MacroTable.t -> state
val get_token_exn : state -> t_token (* may raise exception Error.Error *)
