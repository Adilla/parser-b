open Utils
open Syntax

type env = { gl: Global.t; uf:Unif.t; cl:Global.t_clause }

module Local :
sig
  type t
  val create : unit -> t
  val add : t -> ident -> Btype_mt.t -> bool -> t
  val get : t -> ident -> (Btype_mt.t*bool) option
  val get_vars : t -> ident list
end

type t_var = (loc,Btype.t) var 
type t_expression = (loc,Btype.t) expression
type t_predicate = (loc,Btype.t) predicate
type t_substitution = (loc,Btype.t) substitution

val type_expression  : env -> Local.t -> p_expression -> t_expression Error.t_result
val type_predicate   : env -> Local.t -> p_predicate -> t_predicate Error.t_result
val type_substitution: env -> Local.t -> p_substitution -> t_substitution Error.t_result
