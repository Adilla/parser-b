(** Type inference for expressions, predicate and substitutions *)
open Utils
open Syntax

module Local :
sig
  type t
  val create : unit -> t
  val add : t -> P.ident -> Btype.Open.t -> bool -> t
  val get : t -> P.ident -> (Btype.Open.t*bool) option
  val get_vars : t -> P.ident list
end

val type_expression  : Global.t_clause -> Global.t -> Local.t -> P.expression -> T.expression Error.t_result
val type_predicate   : Global.t_clause -> Global.t -> Local.t -> P.predicate -> T.predicate Error.t_result
val type_substitution: Global.t_clause -> Global.t -> Local.t -> P.substitution -> T.substitution Error.t_result
