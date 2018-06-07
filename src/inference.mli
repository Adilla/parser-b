(** Type inference for expressions, predicate and substitutions *)
open Utils
open Syntax

module Local :
sig
  type t
  val create : unit -> t
  val add : t -> ident -> Btype.Open.t -> bool -> t
  val get : t -> ident -> (Btype.Open.t*bool) option
  val get_vars : t -> ident list
end

type t_var = (loc,Btype.t) var 
type t_expression = (loc,Btype.t) expression
type t_predicate = (loc,Btype.t) predicate
type t_substitution = (loc,Btype.t) substitution

val type_expression  : Global.t_clause -> Global.t -> Local.t -> p_expression -> t_expression Error.t_result
val type_predicate   : Global.t_clause -> Global.t -> Local.t -> p_predicate -> t_predicate Error.t_result
val type_substitution: Global.t_clause -> Global.t -> Local.t -> p_substitution -> t_substitution Error.t_result
