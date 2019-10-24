(** Typechecking for components*)

val allow_becomes_such_that_in_implementation : bool ref
val allow_out_parameters_in_precondition : bool ref

val type_component : (Utils.loc -> string -> Global.t_interface option) -> PSyntax.component ->
  TSyntax.component*Global.t_interface option
