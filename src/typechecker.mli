(** Typechecking for components*)
open Syntax

val allow_becomes_such_that_in_implementation : bool ref
val allow_out_parameters_in_precondition : bool ref

type t_component = (Utils.loc,Btype.t) component

val type_component: (Utils.loc->string -> Global.t_interface option) -> Global.t -> p_component -> t_component Error.t_result
val get_interface: (Utils.loc ->string -> Global.t_interface option) -> Syntax.p_component -> (t_component*Global.t_interface) Error.t_result
