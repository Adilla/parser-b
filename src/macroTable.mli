open Lexing_Utils

type t
val make: string -> Lexing.lexbuf -> t
val get_macro: t -> string -> (Utils.loc*string list*t_token list) option
val print: out_channel -> t -> unit
