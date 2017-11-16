open Utils
open Syntax
open Btype

type env = { gl: Global.t; uf:Unif.t; cl:Global.t_clause }

module Local :
sig
  type t
  val create : unit -> t
  val add : t -> ident -> opn typ -> bool -> t
  val get : t -> ident -> (opn typ*bool) option
  val get_vars : t -> ident list
end

type t_var = (loc,btype) var 
type t_expression = (loc,btype) expression
type t_predicate = (loc,btype) predicate
type t_substitution = (loc,btype) substitution

val type_expression  : env -> Local.t -> p_expression -> t_expression Error.t_result
val type_predicate   : env -> Local.t -> p_predicate -> t_predicate Error.t_result
val type_substitution: env -> Local.t -> p_substitution -> t_substitution Error.t_result
