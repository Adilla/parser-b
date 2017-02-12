type macro_table
type macro

val add_path : string -> unit

val mk_macro_table : string -> in_channel -> (macro_table,Utils.loc*string) result
val dump_table : macro_table -> unit
val find : macro_table -> string -> macro option

val has_parameters : macro -> bool
val expand : Utils.loc -> Lexer_with_lk.state -> macro -> Lexer_with_lk.t_token list list -> (unit,Utils.loc*string) result
