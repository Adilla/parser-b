(* Lexer with look ahead *)
val token_to_string : Grammar.token -> string

type t_token = Grammar.token * Lexing.position * Lexing.position

type state

val mk_state : string -> in_channel -> state
val get_next : state -> Grammar.token * Lexing.position * Lexing.position
val get_last_token_str : state -> string
val get_current_pos : state -> Lexing.position
val prepend_queue : state -> t_token Queue.t -> unit
