(** Typechecking for components*)
open Syntax

val allow_becomes_such_that_in_implementation : bool ref
val allow_out_parameters_in_precondition : bool ref

val type_component: (Utils.loc->string->Global.t_interface option) -> Global.t -> P.component -> T.component Error.t_result
val get_interface: (Utils.loc ->string->Global.t_interface option) -> P.component -> (T.component*Global.t_interface) Error.t_result
