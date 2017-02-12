open Utils
type state
val get_next : state -> Lexer_with_lk.t_token
val mk_state : string -> in_channel -> (state,loc*string) result
(*val get_macro_table : state -> Preproc.macroTable*)
val get_current_pos : state -> Lexing.position
val get_last_token_str : state -> string
