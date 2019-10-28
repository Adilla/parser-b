(** Error type and Error handling functions *)

exception Fatal
val warn : Utils.loc -> string -> unit
val error : Utils.loc -> string -> 'a
