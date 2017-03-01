open Utils
type state
val get_next_exn : state -> Lexer_with_lk.t_token (* may raise exception Lexing_base.Error *)
val mk_state_from_channel_exn : string -> in_channel -> state
val mk_state_from_string_exn : string -> state
val get_current_pos : state -> Lexing.position
val get_last_token_str : state -> string
