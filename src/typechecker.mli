open Syntax

type t_var = (Utils.loc,Btype.btype) var 
type t_set = (Utils.loc,Btype.btype) set 
type t_expression = (Utils.loc,Btype.btype) expression
type t_predicate = (Utils.loc,Btype.btype) predicate
type t_substitution = (Utils.loc,Btype.btype) substitution
type t_operation = (Utils.loc,Btype.btype) operation
type t_component = (Utils.loc,Btype.btype) component

module MachineInterface :
sig
  type t
end

module Global : 
sig
  type t
  val create: unit -> t
end

module Local : 
sig
  type t
  val create: unit -> t
end

val type_expression : Global.t -> Local.t -> p_expression -> t_expression Error.t_result
val type_predicate  : Global.t -> Local.t -> p_predicate  -> t_predicate Error.t_result
val type_substitution  : Global.t -> Local.t -> p_substitution -> t_substitution Error.t_result

val type_component: (string -> MachineInterface.t option) -> Global.t -> p_component -> t_component Error.t_result
val get_interface: (string -> MachineInterface.t option) -> Syntax.p_component -> MachineInterface.t Error.t_result
