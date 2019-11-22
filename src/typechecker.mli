(** Typechecking for components*)

val type_component : (Utils.loc -> string -> Global.t_interface option) -> PSyntax.component ->
  TSyntax.component*Global.t_interface option
