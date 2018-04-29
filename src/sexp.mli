(** S-expression generation*)
open Syntax

type t

val sexp_to_string : t -> string

val sexp_to_channel : out_channel -> t -> unit

val sexp_of_expr : ('lc,'ty) expression -> t

val sexp_of_pred : ('lc,'ty) predicate -> t

val sexp_of_subst : ('lc,'ty) substitution -> t

val sexp_of_component : ('lc,'ty) component -> t
