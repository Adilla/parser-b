(* Lexer with look ahead *)
val token_to_string : Grammar.token -> string

type t_token = Grammar.token * Lexing.position * Lexing.position

type state

val mk_state : string -> Lexing.lexbuf -> state
val get_next_exn : state -> Grammar.token * Lexing.position * Lexing.position (* may raise Lexer_base.Error exception *)
val get_last_token_str : state -> string
val get_current_pos : state -> Lexing.position
val prepend_queue : state -> t_token Queue.t -> unit
