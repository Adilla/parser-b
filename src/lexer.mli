type t_token = Grammar.token * Lexing.position * Lexing.position
type state

val mk_state_from_channel : string -> in_channel -> state Error.t_result
val mk_state_from_string : string -> state Error.t_result

val get_next_exn : state -> t_token (* may raise exception Error.Error *)
val get_current_pos : state -> Lexing.position
val get_last_token_str : state -> string

val set_macro_fuel: int -> unit
